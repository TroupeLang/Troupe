{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Syntactic-variant declaration folding.
--
-- Runs on the Direct AST after atom folding and before pattern-match
-- elimination. It processes @datatype@ declaration groups left to right
-- (sequential scoping, spec §2), normalizes and hashes each group with
-- 'SynVarHash', and rewrites the program term so that constructor
-- occurrences and constructor patterns become tagged tuples / tuple
-- patterns. After this pass the program carries no declaration groups and
-- no 'S.ConPattern'.
--
-- Datatype imports are out of scope for this milestone: this pass handles
-- locally declared datatype groups only. Qualified (dotted) type names and
-- three-segment constructor occurrences are rejected with a
-- "datatype imports are not yet supported" error.
--
-- See @_dev_planning/syntactic-variants/normalization.md@ (§2–§6).
module SynVarFolding ( foldProg ) where

import           Direct
import           Basics (VarName)
import qualified SynVarHash as H
import           TroupePositionInfo (Located(..), PosInf, getLoc)

import           Control.Monad (forM, forM_, when, foldM)
import           Control.Monad.State
import           Control.Monad.Except
import           Data.List (elemIndex, nub, (\\), intercalate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Graph as Graph

------------------------------------------------------------
-- Environment
------------------------------------------------------------

-- | A resolved constructor: its runtime tag, whether it is nullary, and the
-- datatype / constructor names (kept for diagnostics).
data CtorRes = CtorRes
  { crTag      :: String
  , crNullary  :: Bool
  , crDatatype :: String
  , crName     :: String
  }

-- | A resolved datatype: its declared type parameters, its group hash, and
-- its constructors keyed by constructor name.
data DtEntry = DtEntry
  { deParams    :: [String]
  , deGroupHash :: String
  , deCtors     :: Map String CtorRes
  }

-- | The resolution environment accumulated across declaration groups.
data Env = Env
  { envDts  :: Map String DtEntry     -- ^ datatype name -> entry
  , envBare :: Map String [CtorRes]   -- ^ bare constructor name -> candidates
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-- | Fixed primitive type names (spec §2 / §5).
primNames :: [String]
primNames = ["int", "float", "bigint", "bool", "string", "unit"]

------------------------------------------------------------
-- Fresh-name supply
------------------------------------------------------------

-- | Rewriting runs in a state monad supplying fresh, collision-safe names
-- for the constructor lambdas. Names are prefixed with @$@, which the lexer
-- never produces for source identifiers (matching @$arg@, @$input@, etc. in
-- CaseElimination), so they cannot capture user bindings.
type RW = StateT Int (Except String)

fresh :: RW VarName
fresh = do
  n <- get
  put (n + 1)
  return ("$synvar" ++ show n)

------------------------------------------------------------
-- Entry point
------------------------------------------------------------

-- | Fold declaration groups into tags and rewrite the program term. The
-- returned program has an empty group list and no constructor patterns.
foldProg :: Prog -> Except String Prog
foldProg (Prog imports atoms@(Atoms atomNames) groups term) = do
  checkAtomCollisions atomNames groups
  env   <- processGroups emptyEnv groups
  term' <- evalStateT (rewriteLTerm env term) 0
  return (Prog imports atoms [] term')

-- | Atoms are folded before this pass runs, so an atom name would silently
-- take over every occurrence of a same-named constructor or datatype.
-- Reject the collision instead. (Atoms are scheduled for removal; this
-- check exists only while both declaration forms coexist.)
checkAtomCollisions :: [AtomName] -> [SynDataGroup] -> Except String ()
checkAtomCollisions atomNames groups =
  forM_ [ d | SynDataGroup ds <- groups, d <- ds ] $
    \(SynDataDecl _ n ctors) -> do
      when (n `elem` atomNames) $
        throwError ("datatype " ++ n ++ " collides with the atom " ++ n)
      forM_ [ c | SynCtor c _ <- ctors ] $ \c ->
        when (c `elem` atomNames) $
          throwError ("constructor " ++ c ++ " of datatype " ++ n
                      ++ " collides with the atom " ++ c)

------------------------------------------------------------
-- Declaration processing
------------------------------------------------------------

processGroups :: Env -> [SynDataGroup] -> Except String Env
processGroups = foldM processGroup

processGroup :: Env -> SynDataGroup -> Except String Env
processGroup env (SynDataGroup decls) = do
  let dtNames = [ n | SynDataDecl _ n _ <- decls ]
  -- (1) duplicate datatype name within the group
  case firstDup dtNames of
    Just d  -> throwError ("duplicate datatype " ++ d ++ " in declaration group")
    Nothing -> return ()
  -- (1,3) collisions with earlier groups and with primitive / built-in names
  forM_ dtNames $ \n -> do
    when (n `elem` primNames) $
      throwError ("datatype " ++ n ++ " collides with the primitive type name " ++ n)
    when (n == "list") $
      throwError "datatype list collides with the built-in type constructor list"
    when (Map.member n (envDts env)) $
      throwError ("duplicate datatype " ++ n
                  ++ ": redeclaring a datatype from an earlier group is not allowed")
  -- (2) duplicate constructor name within a datatype
  forM_ decls $ \(SynDataDecl _ n ctors) ->
    case firstDup [ c | SynCtor c _ <- ctors ] of
      Just d  -> throwError ("duplicate constructor " ++ d ++ " in datatype " ++ n)
      Nothing -> return ()
  -- same-group members and their parameter counts (for resolution / check 6)
  let sameGroup = Map.fromList [ (n, length ps) | SynDataDecl ps n _ <- decls ]
  -- (4,6) resolve each constructor payload to a type normal form
  resolved <- forM decls $ \(SynDataDecl params n ctors) -> do
    ctors' <- forM ctors $ \(SynCtor c mty) -> do
      mnf <- traverse (resolveTy env sameGroup params n) mty
      return (c, mnf)
    return (n, length params, ctors')
  -- (5) genuineness: a multi-member group must be strongly connected
  checkGenuine resolved
  -- normalize + hash the group
  let hash = H.groupHash resolved
  -- extend the environment with this group's datatypes and constructors
  let paramsOf = Map.fromList [ (n, ps) | SynDataDecl ps n _ <- decls ]
      newEntries =
        [ (n, DtEntry (Map.findWithDefault [] n paramsOf) hash ctorMap)
        | (n, _, ctors) <- resolved
        , let ctorMap = Map.fromList
                [ (c, CtorRes (H.constructorTag hash n c) (mnf == Nothing) n c)
                | (c, mnf) <- ctors ] ]
      env' = env { envDts = foldr (\(n, e) -> Map.insert n e) (envDts env) newEntries }
      bareAdds = [ res | (_, e) <- newEntries, res <- Map.elems (deCtors e) ]
      envBare' = foldr (\res -> Map.insertWith (++) (crName res) [res]) (envBare env') bareAdds
  return env' { envBare = envBare' }

-- | Resolve a surface type expression to its normal form (spec §2 resolution
-- rules, §4 normal form).
resolveTy :: Env -> Map String Int -> [String] -> String -> SynTyExp
          -> Except String H.TyNF
resolveTy env sameGroup params dtName = go
  where
    go (STyVar v) = case elemIndex v params of
      Just i  -> return (H.Var i)
      Nothing -> throwError ("type variable '" ++ v
                             ++ " is not in the parameter list of datatype " ++ dtName)
    go (STyName q) = case q of
      [n] -> resolveName n
      _   -> throwError "datatype imports are not yet supported"
    go (STyProd tys) = H.Prod <$> mapM go tys
    go (STyApp args q) = do
      args' <- mapM go args
      resolveApp (length args) args' q

    -- | Resolve an application @t1 ... tk target@. The target is a built-in
    -- type constructor, a same-group datatype, or a datatype in a previously
    -- hashed group; the applied argument count must equal its declared
    -- parameter count.
    resolveApp k args q = case q of
      ["list"]
        | k == 1    -> return (H.App args (H.RBuiltin "list"))
        | otherwise -> throwError "the built-in type list expects 1 argument"
      [n] -> case Map.lookup n sameGroup of
        Just pc -> checkArity n pc >> return (H.App args (H.RIn n))
        Nothing -> case Map.lookup n (envDts env) of
          Just e  -> let pc = length (deParams e)
                     in checkArity n pc >> return (H.App args (H.RExt (deGroupHash e) n))
          Nothing -> throwError ("unbound type name: " ++ n)
      _ -> throwError "datatype imports are not yet supported"
      where
        checkArity n pc
          | pc == 0   = throwError ("datatype " ++ n ++ " takes no type arguments")
          | k /= pc   = throwError ("datatype " ++ n ++ " expects " ++ arity pc
                                    ++ ", got " ++ show k)
          | otherwise = return ()

    resolveName n
      | n `elem` primNames = return (H.Prim n)
      | n == "list" =
          throwError "the built-in type list must be applied to an argument (e.g. int list)"
      | otherwise = case Map.lookup n sameGroup of
          Just pc
            | pc > 0    -> throwError ("datatype " ++ n ++ " expects " ++ arity pc)
            | otherwise -> return (H.In n)
          Nothing -> case Map.lookup n (envDts env) of
            Just e
              | not (null (deParams e)) ->
                  throwError ("datatype " ++ n ++ " expects "
                              ++ arity (length (deParams e)))
              | otherwise -> return (H.Ext (deGroupHash e) n)
            Nothing -> throwError ("unbound type name: " ++ n)

    -- | Render an expected type-argument count, e.g. @1 type argument@ /
    -- @2 type arguments@.
    arity pc = show pc ++ " type argument" ++ (if pc == 1 then "" else "s")

-- | Genuineness check (spec §3): a group of two or more members must be
-- strongly connected through in-group payload references.
checkGenuine :: H.Group -> Except String ()
checkGenuine group
  | length group < 2 = return ()
  | otherwise =
      case Graph.stronglyConnComp nodes of
        [Graph.CyclicSCC ns] | length ns == length allNames -> return ()
        sccs ->
          let biggest   = longest (map Graph.flattenSCC sccs)
              offenders = [ n | n <- allNames, n `notElem` biggest ]
          in throwError
               ("the 'and' group " ++ braces allNames
                ++ " is not mutually recursive: "
                ++ intercalate ", " offenders
                ++ (if length offenders == 1 then " is" else " are")
                ++ " not mutually recursive with the rest; declare "
                ++ (if length offenders == 1 then "it" else "them")
                ++ " in a separate group before this one.")
  where
    allNames = [ n | (n, _, _) <- group ]
    nodes    = [ (n, n, refsOf ctors) | (n, _, ctors) <- group ]
    refsOf ctors = nub [ r | (_, mnf) <- ctors, r <- maybe [] inRefs mnf ]
    inRefs (H.In n)         = [n]
    inRefs (H.Prod ts)      = concatMap inRefs ts
    inRefs (H.App ts tgt)   = concatMap inRefs ts ++ inRefsTarget tgt
    inRefs _                = []
    inRefsTarget (H.RIn n)  = [n]
    inRefsTarget _          = []
    longest = foldr (\a b -> if length a >= length b then a else b) []
    braces xs = "{" ++ intercalate ", " xs ++ "}"

firstDup :: Eq a => [a] -> Maybe a
firstDup xs = case xs \\ nub xs of
  (d:_) -> Just d
  []    -> Nothing

------------------------------------------------------------
-- Term rewriting
------------------------------------------------------------

rewriteLTerm :: Env -> LTerm -> RW LTerm
rewriteLTerm env (Loc pos t) = Loc pos <$> rewriteTerm env pos t

rewriteTerm :: Env -> PosInf -> Term -> RW Term
rewriteTerm env pos = \case
  Lit l          -> return (Lit l)
  Var x          -> rewriteVar env pos x
  Abs lam        -> Abs <$> rewriteLambda env lam
  Hnd (Handler p mp mg b) ->
    Hnd <$> (Handler <$> rewritePat env p
                     <*> traverse (rewritePat env) mp
                     <*> traverse (rewriteLTerm env) mg
                     <*> rewriteLTerm env b)
  App f as       -> App <$> rewriteLTerm env f <*> mapM (rewriteLTerm env) as
  Let ds b       -> Let <$> mapM (rewriteDecl env) ds <*> rewriteLTerm env b
  Case e arms    -> Case <$> rewriteLTerm env e
                         <*> mapM (\(p, r) -> (,) <$> rewritePat env p
                                                  <*> rewriteLTerm env r) arms
  If a b c       -> If <$> rewriteLTerm env a <*> rewriteLTerm env b <*> rewriteLTerm env c
  Tuple es tag   -> Tuple <$> mapM (rewriteLTerm env) es <*> pure tag
  Record fs      -> Record <$> rewriteFields env fs
  WithRecord e fs-> WithRecord <$> rewriteLTerm env e <*> rewriteFields env fs
  ProjField e f  -> rewriteProj env pos e f
  ProjIdx e i    -> ProjIdx <$> rewriteLTerm env e <*> pure i
  List es        -> List <$> mapM (rewriteLTerm env) es
  ListCons a b   -> ListCons <$> rewriteLTerm env a <*> rewriteLTerm env b
  Bin op a b     -> Bin op <$> rewriteLTerm env a <*> rewriteLTerm env b
  Un op e        -> Un op <$> rewriteLTerm env e
  Seq es         -> Seq <$> mapM (rewriteLTerm env) es
  Error e        -> Error <$> rewriteLTerm env e

rewriteFields :: Env -> LFields -> RW LFields
rewriteFields env = mapM $ \(f, mt) -> (,) f <$> traverse (rewriteLTerm env) mt

rewriteLambda :: Env -> Lambda -> RW Lambda
rewriteLambda env (Lambda ps b) =
  Lambda <$> mapM (rewritePat env) ps <*> rewriteLTerm env b

-- | Bare name in expression position: a constructor occurrence, else an
-- ordinary variable.
rewriteVar :: Env -> PosInf -> VarName -> RW Term
rewriteVar env pos x = case Map.lookup x (envBare env) of
  Just cands -> do res <- uniqueBare pos x cands
                   ctorExpr pos res
  Nothing    -> return (Var x)

-- | Reinterpret @t.f@ as a datatype-qualified constructor when @t@ names a
-- datatype in scope; otherwise it stays a record projection. A three-segment
-- @m.t.c@ whose middle segment is a datatype with constructor @c@ is a
-- datatype-import error.
rewriteProj :: Env -> PosInf -> LTerm -> FieldName -> RW Term
rewriteProj env pos e f = case e of
  Loc _ (Var t)
    | Just entry <- Map.lookup t (envDts env) ->
        case Map.lookup f (deCtors entry) of
          Just res -> ctorExpr pos res
          Nothing  -> throwError ("datatype " ++ t ++ " has no constructor " ++ f)
  Loc _ (ProjField (Loc _ (Var _)) t)
    | Just entry <- Map.lookup t (envDts env)
    , Map.member f (deCtors entry) ->
        throwError "datatype imports are not yet supported"
  _ -> ProjField <$> rewriteLTerm env e <*> pure f

-- | Build the expression for a constructor occurrence: a tagged 1-tuple for a
-- nullary constructor, or a unary lambda that tags its argument otherwise.
ctorExpr :: PosInf -> CtorRes -> RW Term
ctorExpr pos res
  | crNullary res = return (Tuple [tagLit] True)
  | otherwise = do
      v <- fresh
      let body = Loc pos (Tuple [tagLit, Loc pos (Var v)] True)
      return (Abs (Lambda [Loc pos (VarPattern v)] body))
  where tagLit = Loc pos (Lit (LString (crTag res)))

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

rewriteDecl :: Env -> Decl -> RW Decl
rewriteDecl env = \case
  ValDecl p e -> ValDecl <$> rewritePat env p <*> rewriteLTerm env e
  FunDecs fds -> FunDecs <$> mapM (rewriteLFunDecl env) fds
  ErrorDecl   -> return ErrorDecl

rewriteLFunDecl :: Env -> LFunDecl -> RW LFunDecl
rewriteLFunDecl env (Loc p (FunDecl name lams)) = do
  checkBinder name       -- (7) a function name may not shadow a constructor / datatype
  lams' <- mapM (rewriteLambda env) lams
  return (Loc p (FunDecl name lams'))
  where
    checkBinder n = do
      when (Map.member n (envBare env)) $ throwError (n ++ " is a constructor name")
      when (Map.member n (envDts env))  $ throwError (n ++ " is a datatype name")

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

rewritePat :: Env -> LDeclPattern -> RW LDeclPattern
rewritePat env (Loc pos p) = Loc pos <$> rewritePat' env pos p

rewritePat' :: Env -> PosInf -> DeclPattern -> RW DeclPattern
rewritePat' env pos = \case
  VarPattern x
    | Map.member x (envDts env) -> throwError (x ++ " is a datatype name")
    | otherwise -> case Map.lookup x (envBare env) of
        Just cands -> do res <- uniqueBare pos x cands
                         ctorPat env pos res Nothing
        Nothing    -> return (VarPattern x)
  ValPattern l        -> return (ValPattern l)
  Wildcard            -> return Wildcard
  AtPattern p l       -> AtPattern <$> rewritePat env p <*> pure l
  TuplePattern ps     -> TuplePattern <$> mapM (rewritePat env) ps
  ConsPattern a b     -> ConsPattern <$> rewritePat env a <*> rewritePat env b
  ListPattern ps      -> ListPattern <$> mapM (rewritePat env) ps
  RecordPattern fs md -> RecordPattern <$> mapM (rewriteField env) fs <*> pure md
  ConPattern q mp     -> rewriteConPat env pos q mp
  ErrorPattern        -> return ErrorPattern

-- | A record field: a punned field @{x}@ binds @x@, so its name is a binder
-- and subject to check 7; @{x = p}@ names field @x@ and matches @p@.
rewriteField :: Env -> (FieldName, Maybe LDeclPattern)
             -> RW (FieldName, Maybe LDeclPattern)
rewriteField env (f, Nothing) = do
  when (Map.member f (envDts env))  $ throwError (f ++ " is a datatype name")
  when (Map.member f (envBare env)) $ throwError (f ++ " is a constructor name")
  return (f, Nothing)
rewriteField env (f, Just p) = (,) f . Just <$> rewritePat env p

rewriteConPat :: Env -> PosInf -> QName -> Maybe LDeclPattern -> RW DeclPattern
rewriteConPat env pos q mp = case q of
  [c]    -> case Map.lookup c (envBare env) of
              Just cands -> do res <- uniqueBare pos c cands
                               ctorPat env pos res mp
              Nothing    -> throwError ("unbound constructor: " ++ c)
  [t, c] -> case Map.lookup t (envDts env) of
              Just entry -> case Map.lookup c (deCtors entry) of
                Just res -> ctorPat env pos res mp
                Nothing  -> throwError ("datatype " ++ t ++ " has no constructor " ++ c)
              Nothing -> throwError ("datatype " ++ t ++ " is not in scope")
  (_:_:_:_) -> throwError "datatype imports are not yet supported"
  _         -> throwError "malformed constructor pattern"

-- | Build the tuple pattern for a constructor pattern, enforcing arity.
ctorPat :: Env -> PosInf -> CtorRes -> Maybe LDeclPattern -> RW DeclPattern
ctorPat env pos res mp = case mp of
  Nothing
    | crNullary res -> return (TuplePattern [tagPat])
    | otherwise     -> throwError ("constructor " ++ crName res ++ " expects an argument")
  Just p
    | crNullary res -> throwError ("constructor " ++ crName res ++ " takes no argument")
    | otherwise     -> do p' <- rewritePat env p
                          return (TuplePattern [tagPat, p'])
  where tagPat = Loc pos (ValPattern (LString (crTag res)))

------------------------------------------------------------
-- Bare-name disambiguation (spec §2, check 8)
------------------------------------------------------------

uniqueBare :: PosInf -> String -> [CtorRes] -> RW CtorRes
uniqueBare _ _ [res] = return res
uniqueBare _ name cands =
  throwError ("constructor " ++ name ++ " is ambiguous: declared in datatypes "
              ++ intercalate ", " (map crDatatype cands)
              ++ "; qualify it, e.g. " ++ crDatatype (head cands) ++ "." ++ name)
