{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Syntactic-variant declaration folding.
--
-- Runs on the Direct AST before pattern-match
-- elimination. It processes @datatype@ declaration groups left to right
-- (sequential scoping, spec §2), normalizes and hashes each group with
-- 'SynVarHash', and rewrites the program term so that constructor
-- occurrences and constructor patterns become tagged tuples / tuple
-- patterns. After this pass the program carries no declaration groups and
-- no 'S.ConPattern'.
--
-- Datatype imports (spec §10): the interface of an imported library carries,
-- per exported datatype group, its group hash and canonical form. Those enter
-- the resolution environment as groups declared BEFORE all local groups (local
-- groups may shadow them). Imported constructors are usable bare (for
-- unqualified imports) and through the qualified forms @L.c@ / @L.t.c@, and
-- imported datatypes may be referenced in @of@ clauses bare (unqualified) or as
-- @L.t@. The compiler recomputes each imported group's hash from its canonical
-- form, so the interface's stored hash is documentation only; a corrupted
-- interface can only produce non-matching tags, never a forged identity.
--
-- The pass returns, alongside the rewritten program: the (hash, canonical form)
-- of each local group in declaration order (for the library @.exports@ file and
-- the library's embedded exported-hash list), and the group hashes consumed per
-- imported library (for the load-time version-skew check, spec §10).
module SynVarFolding ( foldProg, FoldResult(..) ) where

import           Direct
import           Basics (VarName, Imports(..), ImportDecl(..), ImportMode(..),
                         LibName(..))
import qualified SynVarHash as H
import           TroupePositionInfo (Located(..), PosInf(..), getLoc)

import           Control.Monad (forM, forM_, when, foldM)
import           Control.Monad.State
import           Control.Monad.Except
import           Data.List (elemIndex, nub, nubBy, (\\), intercalate, sort)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Graph as Graph

------------------------------------------------------------
-- Environment
------------------------------------------------------------

-- | A resolved constructor: its runtime tag, whether it is nullary, the
-- datatype / constructor names (kept for diagnostics), and its group hash
-- (used to record cross-library consumption for the load-time skew check).
data CtorRes = CtorRes
  { crTag       :: String
  , crNullary   :: Bool
  , crDatatype  :: String
  , crName      :: String
  , crGroupHash :: String
  }

-- | A resolved datatype: its declared type parameters, its group hash, and
-- its constructors keyed by constructor name.
data DtEntry = DtEntry
  { deParams    :: [String]
  , deGroupHash :: String
  , deCtors     :: Map String CtorRes
  }

-- | The resolution environment accumulated across declaration groups.
--
-- @envDts@ / @envBare@ hold the bare-visible datatypes and constructors:
-- locally declared ones and those from unqualified imports. @envMod@ /
-- @envModCtors@ hold every import's datatypes and constructors keyed by the
-- import's qualifier (its alias, else the library name), enabling the
-- @L.t@ / @L.t.c@ / @L.c@ forms for both qualified and unqualified imports.
data Env = Env
  { envDts            :: Map String DtEntry            -- ^ datatype name -> entry
  , envBare           :: Map String [CtorRes]          -- ^ bare constructor name -> candidates
  , envMod            :: Map String (Map String DtEntry)
      -- ^ qualifier -> (datatype name -> entry)
  , envModCtors       :: Map String (Map String [CtorRes])
      -- ^ qualifier -> (constructor name -> candidates)
  , envImportedHashes :: Set String                    -- ^ all imported group hashes
  , envHashToLib      :: Map String String             -- ^ imported group hash -> library name
  }

-- | Fixed primitive type names (spec §2 / §5).
primNames :: [String]
primNames = ["int", "float", "bigint", "bool", "string", "unit"]

------------------------------------------------------------
-- Rewriting monad
------------------------------------------------------------

-- | Rewriting runs in a state monad supplying fresh, collision-safe names for
-- the constructor lambdas and accumulating the set of imported group hashes
-- consumed by constructor occurrences. Fresh names are prefixed with @$@,
-- which the lexer never produces for source identifiers, so they cannot
-- capture user bindings.
type RW = StateT RWState (Except String)

data RWState = RWState { rwFresh :: Int, rwConsumed :: Set String }

fresh :: RW VarName
fresh = do
  st <- get
  put st { rwFresh = rwFresh st + 1 }
  return ("$synvar" ++ show (rwFresh st))

-- | Record that a constructor from an imported group was used, so the
-- consumed-hash list can drive the load-time version-skew check (spec §10).
noteConsumed :: Env -> CtorRes -> RW ()
noteConsumed env res =
  when (crGroupHash res `Set.member` envImportedHashes env) $
    modify (\st -> st { rwConsumed = Set.insert (crGroupHash res) (rwConsumed st) })

------------------------------------------------------------
-- Entry point
------------------------------------------------------------

-- | The result of folding: the rewritten program, each local group's
-- (hash, canonical form) in declaration order, and the group hashes consumed
-- per imported library.
data FoldResult = FoldResult
  { frProg     :: Prog
  , frLocal    :: [(String, String)]      -- ^ (group hash, canonical form), declaration order
  , frConsumed :: [(String, [String])]    -- ^ (library name, consumed group hashes)
  }

-- | Fold declaration groups into tags and rewrite the program term. The
-- returned program has an empty group list and no constructor patterns.
foldProg :: Prog -> Except String FoldResult
foldProg (Prog imports groups term) = do
  env0                  <- buildImportEnv imports
  (env, localInfos, tyC) <- processGroups env0 groups
  (term', st) <- runStateT (rewriteLTerm env term) (RWState 0 Set.empty)
  let consumed = Set.union tyC (rwConsumed st)
      byLib    = groupConsumedByLib env consumed
  return FoldResult { frProg     = Prog imports [] term'
                    , frLocal    = localInfos
                    , frConsumed = byLib }

-- | Group consumed hashes by the library that supplied them, dropping
-- libraries whose datatypes were not consumed. Hashes and libraries are sorted
-- for deterministic output.
groupConsumedByLib :: Env -> Set String -> [(String, [String])]
groupConsumedByLib env consumed =
  let byLib = Map.fromListWith (++)
                [ (lib, [h])
                | h <- Set.toList consumed
                , Just lib <- [Map.lookup h (envHashToLib env)] ]
  in [ (lib, sort hs) | (lib, hs) <- Map.toAscList byLib ]

------------------------------------------------------------
-- Imported datatype interface (spec §10)
------------------------------------------------------------

-- | The name under which an import is qualified: its alias if present, else the
-- library name. Mirrors the value-import scoping in 'Core.mapFromImports'.
importQualifier :: ImportDecl -> String
importQualifier imp = case importAlias imp of
  Just (LibName a) -> a
  Nothing          -> let LibName l = importLib imp in l

-- | Build the initial environment from the imported libraries' datatype
-- interfaces. Imported groups are treated as declared before every local
-- group. The stored interface hash is ignored; the hash is recomputed from the
-- canonical form (spec §10: the interface is a cache with no authority).
buildImportEnv :: Imports -> Except String Env
buildImportEnv (Imports imports) = foldM addImport emptyEnv imports
  where
    emptyEnv = Env Map.empty Map.empty Map.empty Map.empty Set.empty Map.empty

    addImport env imp = do
      let LibName lib = importLib imp
          qual        = importQualifier imp
          bareVisible = importMode imp == Unqualified
      -- Each interface line is one group's canonical form; parse it and
      -- build the datatype entries, recomputing the group's hash.
      dtEntries <- forM (importDatatypes imp) $ \(_stored, canon) ->
        case H.parseGroup canon of
          Left err -> throwError ("malformed datatype interface for library '"
                                   ++ lib ++ "': " ++ err ++ " in: " ++ canon)
          Right grp -> return (entriesOfGroup grp)
      let entries = concat dtEntries        -- [(dtName, DtEntry)]
          hashes  = [ deGroupHash e | (_, e) <- entries ]
          env1 = env
            { envImportedHashes = foldr Set.insert (envImportedHashes env) hashes
            , envHashToLib = foldr (\h -> Map.insertWith (\_ old -> old) h lib)
                                   (envHashToLib env) hashes
            , envMod = Map.insertWith Map.union qual (Map.fromList entries) (envMod env)
            , envModCtors = Map.insertWith (Map.unionWith (++)) qual
                              (ctorMapOf entries) (envModCtors env)
            }
      -- Unqualified imports also make their datatypes and constructors visible
      -- bare, as groups declared before all local groups.
      return $ if bareVisible
               then env1 { envDts  = Map.union (envDts env1) (Map.fromList entries)
                         , envBare = Map.unionWith (++) (envBare env1)
                                       (bareCtorsOf entries) }
               else env1

    -- | Turn a parsed group into datatype entries. The group's hash is
    -- recomputed here from its canonical form.
    entriesOfGroup :: H.Group -> [(String, DtEntry)]
    entriesOfGroup grp =
      let h = H.groupHash grp
      in [ (dtName, DtEntry (replicate nparams "_") h ctors)
         | (dtName, nparams, cs) <- grp
         , let ctors = Map.fromList
                 [ (cn, CtorRes (H.constructorTag h dtName cn)
                                (payload == Nothing) dtName cn h)
                 | (cn, payload) <- cs ] ]

    ctorMapOf :: [(String, DtEntry)] -> Map String [CtorRes]
    ctorMapOf entries = Map.fromListWith (++)
      [ (crName res, [res]) | (_, e) <- entries, res <- Map.elems (deCtors e) ]

    bareCtorsOf :: [(String, DtEntry)] -> Map String [CtorRes]
    bareCtorsOf = ctorMapOf

------------------------------------------------------------
-- Declaration processing
------------------------------------------------------------

-- | Process local declaration groups left to right, threading the environment,
-- the accumulating (hash, canonical form) list, and the set of imported group
-- hashes consumed through @of@-clause references to imported types.
processGroups :: Env -> [SynDataGroup]
              -> Except String (Env, [(String, String)], Set String)
processGroups env0 = foldM step (env0, [], Set.empty)
  where
    step (env, infos, consumed) grp = do
      (env', info, c) <- processGroup env grp
      return (env', infos ++ [info], Set.union consumed c)

processGroup :: Env -> SynDataGroup
             -> Except String (Env, (String, String), Set String)
processGroup env (SynDataGroup decls) = do
  let dtNames = [ (n, dp) | SynDataDecl dp _ n _ <- decls ]
  -- (1) duplicate datatype name within the group: the members are
  -- simultaneously in scope, so there is no "nearest" to prefer. Shadowing a
  -- name from an earlier group, a primitive, or a built-in is legal lexical
  -- shadowing (spec §2) and is *not* checked here. Both occurrences are cited.
  case firstDupPos dtNames of
    Just (d, p0, p1) ->
      throwError (at p1 ("duplicate datatype " ++ d ++ " in declaration group"
                         ++ firstAt p0))
    Nothing -> return ()
  -- (2) duplicate constructor name within a datatype; both occurrences are cited
  forM_ decls $ \(SynDataDecl _ _ n ctors) ->
    case firstDupPos [ (c, cp) | SynCtor cp c _ <- ctors ] of
      Just (d, p0, p1) ->
        throwError (at p1 ("duplicate constructor " ++ d ++ " in datatype " ++ n
                           ++ firstAt p0))
      Nothing -> return ()
  -- same-group members and their parameter counts (for resolution / check 6)
  let sameGroup = Map.fromList [ (n, length ps) | SynDataDecl _ ps n _ <- decls ]
  -- (4,6) resolve each constructor payload to a type normal form; payload
  -- resolution errors are reported against the enclosing constructor's name.
  resolved <- forM decls $ \(SynDataDecl _ params n ctors) -> do
    ctors' <- forM ctors $ \(SynCtor cp c mty) -> do
      mnf <- traverse (resolveTy env sameGroup params n cp) mty
      return (c, mnf)
    return (n, length params, ctors')
  -- (5) genuineness: a multi-member group must be strongly connected
  let dtPos = Map.fromList dtNames
  checkGenuine dtPos resolved
  -- normalize + hash the group
  let canon = H.canonicalGroup resolved
      hash  = H.groupHash resolved
  -- imported group hashes referenced by this group's payloads are consumed
  let consumed = Set.fromList
        [ h | (_, _, ctors) <- resolved, (_, mnf) <- ctors, Just nf <- [mnf]
            , h <- extHashes nf, h `Set.member` envImportedHashes env ]
  -- extend the environment with this group's datatypes and constructors
  let paramsOf = Map.fromList [ (n, ps) | SynDataDecl _ ps n _ <- decls ]
      newEntries =
        [ (n, DtEntry (Map.findWithDefault [] n paramsOf) hash ctorMap)
        | (n, _, ctors) <- resolved
        , let ctorMap = Map.fromList
                [ (c, CtorRes (H.constructorTag hash n c) (mnf == Nothing) n c hash)
                | (c, mnf) <- ctors ] ]
      env' = env { envDts = foldr (\(n, e) -> Map.insert n e) (envDts env) newEntries }
      bareAdds = [ res | (_, e) <- newEntries, res <- Map.elems (deCtors e) ]
      envBare' = foldr (\res -> Map.insertWith (++) (crName res) [res]) (envBare env') bareAdds
  return (env' { envBare = envBare' }, (hash, canon), consumed)

-- | Collect the group hashes appearing in @ext@ / @app ... ext@ nodes of a
-- payload normal form (the imported-type references).
extHashes :: H.TyNF -> [String]
extHashes = \case
  H.Ext h _   -> [h]
  H.Prod ts   -> concatMap extHashes ts
  H.App ts tg -> concatMap extHashes ts ++ targetHashes tg
  _           -> []
  where
    targetHashes (H.RExt h _) = [h]
    targetHashes _            = []

-- | Resolve a surface type expression to its normal form (spec §2 resolution
-- rules, §4 normal form).
resolveTy :: Env -> Map String Int -> [String] -> String -> PosInf -> SynTyExp
          -> Except String H.TyNF
resolveTy env sameGroup params dtName cpos = go
  where
    go (STyVar v) = case elemIndex v params of
      Just i  -> return (H.Var i)
      Nothing -> throwHere ("type variable '" ++ v
                             ++ " is not in the parameter list of datatype " ++ dtName)
    go (STyName q) = resolveName q
    go (STyProd tys) = H.Prod <$> mapM go tys
    go (STyApp args q) = do
      args' <- mapM go args
      resolveApp (length args) args' q

    throwHere msg = throwError (at cpos msg)

    -- | Resolve an application @t1 ... tk target@. The target is a built-in
    -- type constructor, a same-group datatype, a bare-visible datatype in a
    -- previously hashed group, or a module-qualified imported datatype (@L.t@);
    -- the applied argument count must equal its declared parameter count.
    resolveApp k args q = case q of
      [n] -> case Map.lookup n sameGroup of
        Just pc -> checkArity n pc >> return (H.App args (H.RIn n))
        Nothing -> case Map.lookup n (envDts env) of
          Just e  -> let pc = length (deParams e)
                     in checkArity n pc >> return (H.App args (H.RExt (deGroupHash e) n))
          Nothing
            | n == "list" && k == 1 -> return (H.App args (H.RBuiltin "list"))
            | n == "list"           -> throwHere "the built-in type list expects 1 argument"
            | otherwise             -> throwHere ("unbound type name: " ++ n)
      [m, t] -> do
        e <- lookupQualifiedTy m t
        let pc = length (deParams e)
        checkArity t pc
        return (H.App args (H.RExt (deGroupHash e) t))
      _ -> throwHere ("malformed qualified type name: " ++ intercalate "." q)
      where
        checkArity n pc
          | pc == 0   = throwHere ("datatype " ++ n ++ " takes no type arguments")
          | k /= pc   = throwHere ("datatype " ++ n ++ " expects " ++ arity pc
                                    ++ ", got " ++ show k)
          | otherwise = return ()

    -- Resolution order (spec §2): the nearest datatype in scope shadows a
    -- primitive or built-in of the same name. Same-group members are nearest,
    -- then the accumulated environment (bare-visible local + unqualified
    -- imports, later groups winning), then the fixed primitive / built-in set,
    -- then unbound. A qualified name @L.t@ resolves through the import
    -- qualifier only.
    resolveName [n] = case Map.lookup n sameGroup of
      Just pc
        | pc > 0    -> throwHere ("datatype " ++ n ++ " expects " ++ arity pc)
        | otherwise -> return (H.In n)
      Nothing -> case Map.lookup n (envDts env) of
        Just e
          | not (null (deParams e)) ->
              throwHere ("datatype " ++ n ++ " expects "
                          ++ arity (length (deParams e)))
          | otherwise -> return (H.Ext (deGroupHash e) n)
        Nothing
          | n `elem` primNames -> return (H.Prim n)
          | n == "list" ->
              throwHere "the built-in type list must be applied to an argument (e.g. int list)"
          | otherwise -> throwHere ("unbound type name: " ++ n)
    resolveName [m, t] = do
      e <- lookupQualifiedTy m t
      if not (null (deParams e))
        then throwHere ("datatype " ++ m ++ "." ++ t ++ " expects "
                         ++ arity (length (deParams e)))
        else return (H.Ext (deGroupHash e) t)
    resolveName q = throwHere ("malformed qualified type name: " ++ intercalate "." q)

    -- | Resolve @m.t@ against the imported datatypes of qualifier @m@.
    lookupQualifiedTy m t = case Map.lookup m (envMod env) of
      Nothing -> throwHere ("no imported library qualified as " ++ m)
      Just dts -> case Map.lookup t dts of
        Nothing -> throwHere ("library " ++ m ++ " has no datatype " ++ t)
        Just e  -> return e

    -- | Render an expected type-argument count, e.g. @1 type argument@ /
    -- @2 type arguments@.
    arity pc = show pc ++ " type argument" ++ (if pc == 1 then "" else "s")

-- | Genuineness check (spec §3): a group of two or more members must be
-- strongly connected through in-group payload references.
checkGenuine :: Map String PosInf -> H.Group -> Except String ()
checkGenuine dtPos group
  | length group < 2 = return ()
  | otherwise =
      case Graph.stronglyConnComp nodes of
        [Graph.CyclicSCC ns] | length ns == length allNames -> return ()
        sccs ->
          let biggest   = longest (map Graph.flattenSCC sccs)
              offenders = [ n | n <- allNames, n `notElem` biggest ]
              offPos    = case offenders of
                            (o:_) -> Map.findWithDefault NoPos o dtPos
                            []    -> NoPos
          in throwError $ at offPos
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

-- | Find the first repeated element in a positioned list, returning the value
-- together with the position of its first occurrence and the position of the
-- repeat. Used to cite both occurrences in duplicate diagnostics.
firstDupPos :: Eq a => [(a, PosInf)] -> Maybe (a, PosInf, PosInf)
firstDupPos = go []
  where
    go _ [] = Nothing
    go seen ((x, p) : rest) = case lookup x seen of
      Just p0 -> Just (x, p0, p)
      Nothing -> go (seen ++ [(x, p)]) rest

------------------------------------------------------------
-- Diagnostic positions
------------------------------------------------------------

-- | Prefix a message with a source position, matching the compiler's error
-- convention (@FILE:ROW:COL: message@). A position-less node yields the bare
-- message.
at :: PosInf -> String -> String
at NoPos msg = msg
at p      msg = show p ++ ": " ++ msg

-- | A parenthetical citing an earlier occurrence, e.g. for the first of two
-- duplicate declarations.
firstAt :: PosInf -> String
firstAt NoPos = ""
firstAt p     = " (first declared at " ++ show p ++ ")"

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
                   noteConsumed env res
                   ctorExpr pos res
  Nothing    -> return (Var x)

-- | Reinterpret dotted expressions as constructor occurrences where a segment
-- names a datatype or an import qualifier (spec §2, §10):
--
--   * @t.c@   — datatype-qualified constructor (t a bare-visible datatype);
--   * @L.c@   — module-qualified bare constructor (L an import qualifier);
--   * @L.t.c@ — module-qualified, datatype-qualified constructor.
--
-- Anything else stays a record projection.
rewriteProj :: Env -> PosInf -> LTerm -> FieldName -> RW Term
rewriteProj env pos e f = case e of
  -- t.c where t is a bare-visible datatype: definitively a constructor form,
  -- so a missing constructor is an error (a datatype name is not a value).
  Loc _ (Var t)
    | Just entry <- Map.lookup t (envDts env) ->
        case Map.lookup f (deCtors entry) of
          Just res -> noteConsumed env res >> ctorExpr pos res
          Nothing  -> throwError (at pos ("datatype " ++ t ++ " has no constructor " ++ f))
  -- L.c where L is an import qualifier and c is one of its constructors. The
  -- qualifier namespace overlaps value/module access (e.g. Number.intDiv), so
  -- this fires only when c is actually a constructor of L; otherwise it falls
  -- through to ordinary projection. A constructor ambiguous across L's
  -- datatypes is a use-site error (uniqueBare).
  Loc _ (Var m)
    | Just cands <- moduleCands env m f -> do
        res <- uniqueBare pos f cands
        noteConsumed env res
        ctorExpr pos res
  -- L.t.c where L is an import qualifier and t one of its datatypes: a
  -- datatype has no further projection, so once L and t match this is
  -- definitively a constructor form and a missing constructor is an error.
  -- (When t is not a datatype of L, e.g. L.value.field, this falls through.)
  Loc _ (ProjField (Loc _ (Var m)) t)
    | Just dts <- Map.lookup m (envMod env)
    , Just entry <- Map.lookup t dts ->
        case Map.lookup f (deCtors entry) of
          Just res -> noteConsumed env res >> ctorExpr pos res
          Nothing  -> throwError (at pos ("datatype " ++ m ++ "." ++ t
                                          ++ " has no constructor " ++ f))
  _ -> ProjField <$> rewriteLTerm env e <*> pure f

-- | The candidates for a module-qualified bare constructor @m.f@: the
-- constructors named @f@ among the datatypes of import qualifier @m@. Returns
-- 'Nothing' when @m@ is not a qualifier or @f@ is not one of its constructors,
-- so callers fall through to value/record projection.
moduleCands :: Env -> String -> String -> Maybe [CtorRes]
moduleCands env m f = Map.lookup m (envModCtors env) >>= Map.lookup f

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
      when (Map.member n (envBare env)) $ throwError (at p (n ++ " is a constructor name"))
      when (Map.member n (envDts env))  $ throwError (at p (n ++ " is a datatype name"))

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

rewritePat :: Env -> LDeclPattern -> RW LDeclPattern
rewritePat env (Loc pos p) = Loc pos <$> rewritePat' env pos p

rewritePat' :: Env -> PosInf -> DeclPattern -> RW DeclPattern
rewritePat' env pos = \case
  VarPattern x
    | Map.member x (envDts env) -> throwError (at pos (x ++ " is a datatype name"))
    | otherwise -> case Map.lookup x (envBare env) of
        Just cands -> do res <- uniqueBare pos x cands
                         noteConsumed env res
                         ctorPat env pos res Nothing
        Nothing    -> return (VarPattern x)
  ValPattern l        -> return (ValPattern l)
  Wildcard            -> return Wildcard
  AtPattern p l       -> AtPattern <$> rewritePat env p <*> pure l
  TuplePattern ps     -> TuplePattern <$> mapM (rewritePat env) ps
  ConsPattern a b     -> ConsPattern <$> rewritePat env a <*> rewritePat env b
  ListPattern ps      -> ListPattern <$> mapM (rewritePat env) ps
  RecordPattern fs md -> RecordPattern <$> mapM (rewriteField env pos) fs <*> pure md
  ConPattern q mp     -> rewriteConPat env pos q mp
  ErrorPattern        -> return ErrorPattern

-- | A record field: a punned field @{x}@ binds @x@, so its name is a binder
-- and subject to check 7; @{x = p}@ names field @x@ and matches @p@.
rewriteField :: Env -> PosInf -> (FieldName, Maybe LDeclPattern)
             -> RW (FieldName, Maybe LDeclPattern)
rewriteField env pos (f, Nothing) = do
  when (Map.member f (envDts env))  $ throwError (at pos (f ++ " is a datatype name"))
  when (Map.member f (envBare env)) $ throwError (at pos (f ++ " is a constructor name"))
  return (f, Nothing)
rewriteField env _ (f, Just p) = (,) f . Just <$> rewritePat env p

-- | A constructor pattern. Segments (spec §2, §10):
--
--   * @[c]@       — bare constructor;
--   * @[t, c]@    — datatype-qualified (t a bare-visible datatype) or
--                   module-qualified bare (t an import qualifier);
--   * @[m, t, c]@ — module-qualified, datatype-qualified.
rewriteConPat :: Env -> PosInf -> QName -> Maybe LDeclPattern -> RW DeclPattern
rewriteConPat env pos q mp = case q of
  [c] -> case Map.lookup c (envBare env) of
    Just cands -> do res <- uniqueBare pos c cands
                     noteConsumed env res
                     ctorPat env pos res mp
    Nothing    -> throwError (at pos ("unbound constructor: " ++ c))
  [t, c]
    | Just entry <- Map.lookup t (envDts env) ->
        case Map.lookup c (deCtors entry) of
          Just res -> noteConsumed env res >> ctorPat env pos res mp
          Nothing  -> throwError (at pos ("datatype " ++ t ++ " has no constructor " ++ c))
    | Just ctors <- Map.lookup t (envModCtors env) ->
        case Map.lookup c ctors of
          Just cands -> do res <- uniqueBare pos c cands
                           noteConsumed env res
                           ctorPat env pos res mp
          Nothing    -> throwError (at pos ("library " ++ t ++ " has no constructor " ++ c))
    | otherwise -> throwError (at pos ("datatype " ++ t ++ " is not in scope"))
  [m, t, c] -> case Map.lookup m (envMod env) of
    Nothing  -> throwError (at pos ("no imported library qualified as " ++ m))
    Just dts -> case Map.lookup t dts of
      Nothing    -> throwError (at pos ("library " ++ m ++ " has no datatype " ++ t))
      Just entry -> case Map.lookup c (deCtors entry) of
        Just res -> noteConsumed env res >> ctorPat env pos res mp
        Nothing  -> throwError (at pos ("datatype " ++ m ++ "." ++ t
                                        ++ " has no constructor " ++ c))
  _ -> throwError (at pos "malformed constructor pattern")

-- | Build the tuple pattern for a constructor pattern, enforcing arity.
ctorPat :: Env -> PosInf -> CtorRes -> Maybe LDeclPattern -> RW DeclPattern
ctorPat env pos res mp = case mp of
  Nothing
    | crNullary res -> return (TuplePattern [tagPat])
    | otherwise     -> throwError (at pos ("constructor " ++ crName res ++ " expects an argument"))
  Just p
    | crNullary res -> throwError (at pos ("constructor " ++ crName res ++ " takes no argument"))
    | otherwise     -> do p' <- rewritePat env p
                          return (TuplePattern [tagPat, p'])
  where tagPat = Loc pos (ValPattern (LString (crTag res)))

------------------------------------------------------------
-- Bare-name disambiguation (spec §2, check 8)
------------------------------------------------------------

-- | Ambiguity is tag-based (spec §10): a bare constructor name is ambiguous
-- only when *distinct tags* compete for it. Candidates that share a tag —
-- identical re-declarations, diamond imports — collapse to one and resolve
-- silently. Distinct tags remain a static error resolved by qualification.
uniqueBare :: PosInf -> String -> [CtorRes] -> RW CtorRes
uniqueBare pos name cands = case nubBy sameTag cands of
  [res] -> return res
  distinct ->
    throwError (at pos ("constructor " ++ name ++ " is ambiguous: declared in datatypes "
                ++ intercalate ", " (map crDatatype distinct)
                ++ "; qualify it, e.g. " ++ crDatatype (head distinct) ++ "." ++ name))
  where sameTag a b = crTag a == crTag b
