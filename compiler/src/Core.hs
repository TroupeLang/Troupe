{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Core (   Lambda (..)
              , Term (..)
              , LTerm
              , Decl (..)
              , LDecl
              , FunDecl (..)
              , LFields
              , Numeric(..)
              , Lit(..)
              , litEq
              , litNeq
              , Prog(..)
              , VarAccess(..)
              , lowerProg
              , renameProg
              , ppLit
              )
where
import GHC.Generics(Generic)
import Data.Serialize (Serialize)
import Sexp

import           Basics
import qualified DirectWOPats as D
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad
import           Control.Monad.State.Lazy as State
import           Control.Monad.RWS
import           Control.Monad.Except

import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.PrettyPrint.HughesPJ (
   (<+>), ($$), text, vcat, nest)
import           ShowIndent

import           TroupePositionInfo (Located(..), getLoc, PosInf(..))
import           PrettyPrint (PP, runPP, runPPDefault, ppLocated, ShowDebug(..))
import           DCLabels (DCLabelExp, ppDCLabelExpLit, dcLabelEq, v1LabelEq, v1LabelToDCLabelExp)

--------------------------------------------------
-- AST is the same as Direct, but lambda are unary (or nullary)

-- | Located type aliases
type LTerm = Located Term
type LDecl = Located Decl
type LFields = [(FieldName, LTerm)]
-- | Located variable name - carries source position for variable bindings
type LVarName = Located VarName

data Lambda = Unary LVarName LTerm
            | Nullary LTerm
  deriving (Eq)

data Decl
    = ValDecl VarName LTerm
    | FunDecs [FunDecl]
  deriving (Eq )

data FunDecl = FunDecl VarName Lambda PosInf  -- Keep PosInf for function definition position
  deriving (Eq)

-- Numeric type represents integer and floating point numeric literals
-- with cross-type equality (NumInt 3 == NumFloat 3.0)
data Numeric = NumInt Integer | NumFloat Double
  deriving (Show, Generic)
instance Serialize Numeric
instance Eq Numeric where
  (NumInt x) == (NumInt y) = x == y
  (NumFloat x) == (NumFloat y) = x == y
  (NumInt x) == (NumFloat y) = fromInteger x == y
  (NumFloat x) == (NumInt y) = x == fromInteger y
instance Ord Numeric where
  compare (NumInt x) (NumInt y) = compare x y
  compare (NumFloat x) (NumFloat y) = compare x y
  compare (NumInt x) (NumFloat y) = compare (fromInteger x) y
  compare (NumFloat x) (NumInt y) = compare x (fromInteger y)

data Lit
    = LNumeric Numeric
    | LString String
    | LLabel String
    | LDCLabel DCLabelExp
    | LUnit
    | LBool Bool
  deriving (Show, Generic)
instance Serialize Lit
instance Eq Lit where
  (LNumeric n1) == (LNumeric n2) = n1 == n2
  (LString s) == (LString s') = s == s'
  (LLabel l) == (LLabel l') = l == l'
  LUnit == LUnit = True
  (LBool x) == (LBool y) = x == y
  (LDCLabel dc) == (LDCLabel dc') = dc == dc'
  _ == _ = False
instance Ord Lit where
  compare (LNumeric n1) (LNumeric n2) = compare n1 n2
  compare (LString x) (LString y) = compare x y
  compare (LLabel x) (LLabel y) = compare x y
  compare LUnit LUnit = EQ
  compare (LBool x) (LBool y) = compare x y
  compare (LDCLabel x) (LDCLabel y) = compare x y
  -- Cross-type ordering (for canonical ordering of different literal types)
  compare (LNumeric _) _ = LT
  compare _ (LNumeric _) = GT
  compare (LString _) _ = LT
  compare _ (LString _) = GT
  compare (LLabel _) _ = LT
  compare _ (LLabel _) = GT
  compare LUnit _ = LT
  compare _ LUnit = GT
  compare (LBool _) _ = LT
  compare _ (LBool _) = GT

-- Note: Lit no longer has embedded position info. Position comes from the
-- Located wrapper around terms containing literals.

-- | Semantic equality for literals, handling label normalization
-- This is used for compile-time constant folding to ensure that
-- semantically equivalent labels (e.g., `{alice, bob}` and `{bob, alice}`)
-- are treated as equal.
litEq :: Lit -> Lit -> Bool
litEq (LNumeric n1) (LNumeric n2) = n1 == n2
litEq (LString s) (LString s') = s == s'
litEq (LLabel l) (LLabel l') = v1LabelEq l l'
litEq LUnit LUnit = True
litEq (LBool x) (LBool y) = x == y
litEq (LDCLabel dc) (LDCLabel dc') = dcLabelEq dc dc'
-- Cross-syntax comparison: V1 labels vs DC labels
litEq (LLabel l) (LDCLabel dc) = dcLabelEq (v1LabelToDCLabelExp l) dc
litEq (LDCLabel dc) (LLabel l) = dcLabelEq dc (v1LabelToDCLabelExp l)
litEq _ _ = False

-- | Semantic inequality for literals
litNeq :: Lit -> Lit -> Bool
litNeq x y = not (litEq x y)

data VarAccess
    -- | A normal variable
    = RegVar VarName
    -- | Referring to a definition from a library
    | LibVar LibName VarName
    -- | A predefined name (e.g. send, receive)
    | BaseName VarName
 deriving (Eq)

-- | Core Term without embedded positions - positions are in Located wrapper
data Term
    = Lit Lit
    | Var VarAccess
    | Abs Lambda
    | App LTerm LTerm
    | Let Decl LTerm
    | If LTerm LTerm LTerm
    | AssertElseError LTerm LTerm LTerm
    | Tuple [LTerm] SynVariantTag
    | Record LFields
    | WithRecord LTerm LFields
    | ProjField LTerm FieldName
    | ProjIdx LTerm Word
    | List [LTerm]
    | ListCons LTerm LTerm
    | Bin BinOp LTerm LTerm
    | Un UnaryOp LTerm
    | Error LTerm
  deriving (Eq)


data Prog = Prog Imports LTerm
  deriving (Eq, Show)

-- Note: GetPosInfo instance for LTerm comes from TroupePositionInfo's
-- instance GetPosInfo (Located a) which extracts position from Loc wrapper


{--

This module defines the Core front-level intermediate representation,
and includes two phases of the compilation pipeline that involve that
representation.

1. Lowering of the program from the direct representation into the
Core representation

2. α-renaming and library name resolution. This is done in the Core
representation.

The module also contains pretty printing for the Core representation.


--}


--------------------------------------------------
-- 1. Lowering
--------------------------------------------------

lowerProg (D.Prog imports lterm) = Prog imports (lower lterm)



-- the rest of the declarations in this part are not exported

-- | Lower a lambda, producing Located terms for nested abstractions
lowerLam :: D.Lambda -> Lambda
lowerLam (D.Lambda vs lt) =
  case vs of
    [] -> Unary (Loc NoPos "$unit") (lower lt)
    lx:xs -> Unary lx (foldr (\lx' b -> Loc (getLoc lt) (Abs (Unary lx' b))) (lower lt) xs)

-- | Lower a literal. Position info is on the Located wrapper, not in the literal.
lowerLit :: D.Lit -> Lit
lowerLit (D.LNumeric n) = LNumeric (lowerNumeric n)
  where
    lowerNumeric (D.NumInt i) = NumInt i
    lowerNumeric (D.NumFloat f) = NumFloat f
lowerLit (D.LString s) = LString s
lowerLit (D.LLabel s) = LLabel s
lowerLit (D.LDCLabel dc) = LDCLabel dc
lowerLit D.LUnit = LUnit
lowerLit (D.LBool b) = LBool b

-- | Lower DirectWOPats.LTerm (Located Term) to Core.LTerm
-- Position is now extracted from the Located wrapper
lower :: D.LTerm -> LTerm
lower (Loc pos (D.Lit l)) = Loc pos (Lit (lowerLit l))
lower (Loc pos (D.Error lt)) = Loc pos (Error (lower lt))
lower (Loc pos (D.Var v)) = Loc pos (Var (RegVar v))
  -- 2018-07-01: AA: note that we are mapping all vars to RegVar at
  -- this stage. This is a bit of a hack. A cleaner apporach is to
  -- have a separate intermediate representation. For now we save on
  -- the engineering effort and proceed like this, because at the
  -- subsequent phase (renaming) we resolve which names are base
  -- names, which are lib names, and which are actually just regular
  -- variables.

lower (Loc pos (D.Abs lam)) = Loc pos (Abs (lowerLam lam))

lower (Loc pos (D.App le [])) = Loc pos (Core.App (lower le) (Loc NoPos (Lit LUnit))) -- does this form even exist?
lower (Loc pos (D.App le les)) = foldl (\acc lt -> Loc pos (Core.App acc (lower lt))) (lower le) les
lower (Loc pos (D.Let decls le)) =
  foldr (\decl t -> Loc pos (Let (lowerDecl decl) t)) (lower le) decls
  where lowerDecl (D.ValDecl vname le') = ValDecl vname (lower le')
        lowerDecl (D.FunDecs decs) = FunDecs (map lowerFun decs)
        lowerFun  (D.FunDecl v lam funPos) = FunDecl v (lowerLam lam) funPos
-- lower (D.Case t patTermLst) = Case (lower t) (map (\(p,t) -> (lowerDeclPat p, lower t)) patTermLst)
lower (Loc pos (D.If le1 le2 le3)) = Loc pos (If (lower le1) (lower le2) (lower le3))
lower (Loc pos (D.AssertElseError le1 le2 le3)) = Loc pos (AssertElseError (lower le1) (lower le2) (lower le3))
lower (Loc pos (D.Tuple lterms tag)) = Loc pos (Tuple (map lower lterms) tag)
lower (Loc pos (D.Record lfields)) = Loc pos (Record (map (\(f, lt) -> (f, lower lt)) lfields))
lower (Loc pos (D.WithRecord le lfields)) = Loc pos (WithRecord (lower le) (map (\(f, lt) -> (f, lower lt)) lfields))
lower (Loc pos (D.ProjField lt f)) = Loc pos (ProjField (lower lt) f)
lower (Loc pos (D.ProjIdx lt idx)) = Loc pos (ProjIdx (lower lt) idx)
lower (Loc pos (D.List lterms)) = Loc pos (List (map lower lterms))
lower (Loc pos (D.ListCons lt1 lt2)) = Loc pos (ListCons (lower lt1) (lower lt2))

-- special casing shortcutting semantics; 2018-03-06;
lower (Loc pos (D.Bin And le1 le2)) = lower (Loc pos (D.If le1 le2 (Loc NoPos (D.Lit (D.LBool False)))))
lower (Loc pos (D.Bin Or le1 le2)) = lower (Loc pos (D.If le1 (Loc NoPos (D.Lit (D.LBool True))) le2))
lower (Loc pos (D.Bin op le1 le2)) = Loc pos (Bin op (lower le1) (lower le2))
lower (Loc pos (D.Un op le)) = Loc pos (Un op (lower le))


--------------------------------------------------
-- 2. α-RENAMING
--------------------------------------------------


-- This is the only function that is exported here

renameProg :: Prog -> Except String Prog
renameProg (Prog imports term) =
  let initEnv    = Map.empty
      initReader = mapFromImports imports
      initState  = 0
  in do
      (term', _, _) <- runRWST (rename term initEnv) initReader initState
      return $ Prog imports term'

-- The rest of the declarations here are not exported

{--

The renaming occurs in RWS monad that is instantiated as follows:

* The reader is the library environment
* The state is the unique variable counter
* The output is not used so we instantiate it to a dummy unit type

Note that the environment used for tracking α-substitutions is being
threaded explicitly. That is encoded in the `Env` map.

--}


type S = RWST LibEnv () Integer (Except String)

-- | Environment for unqualified imports: maps function names to their library
type UnqualifiedLibEnv = Map.Map VarName LibName
-- | Map from effective library name (alias or original) to:
--   (original library name for codegen, set of available exports)
-- Used for resolving and validating A.foo() syntax
type LibExports = Map.Map LibName (LibName, Set.Set VarName)
-- | Combined environment for the Reader monad
type LibEnv = (UnqualifiedLibEnv, LibExports)

type Env    = Map.Map VarName VarName


mapFromImports :: Imports -> LibEnv
mapFromImports (Imports imports) =
  let
    -- Get effective exports: use selected if specified, otherwise all exports
    effectiveExports imp = case importSelected imp of
      Just selected -> selected
      Nothing -> case importExports imp of
        Just exports -> exports
        Nothing -> []

    -- Get effective name for qualified access: alias if present, otherwise original
    effectiveName imp = case importAlias imp of
      Just alias -> alias
      Nothing -> importLib imp

    -- The name codegen addresses the import by: the library name, or the
    -- content-addressed module identity ("module:<hash>") for a module import.
    -- ProcessImports has, by this point, resolved a module import's path to its
    -- IR hash and stored it in importPath, so the reference is location-free and
    -- Merkle (it carries the dependency's hash inline).
    codegenName imp = case importPath imp of
      Just hash -> LibName ("module:" ++ hash)
      Nothing   -> importLib imp

    -- Build unqualified environment (only unqualified imports)
    -- Maps each exported function name to the original library
    unqualifiedImports = [imp | imp <- imports, importMode imp == Unqualified]
    unqualEnv = foldl insLib Map.empty unqualifiedImports
      where
        insLib m imp =
          let lib = codegenName imp
              defs = effectiveExports imp
          in foldl (\m' def -> Map.insert def lib m') m defs

    -- Build map from effective name (alias or original) to (original lib, effective exports)
    -- This is used for resolving and validating A.foo() syntax
    libExports = Map.fromList
      [ (effectiveName imp, (codegenName imp, Set.fromList (effectiveExports imp)))
      | imp <- imports
      ]
  in
    (unqualEnv, libExports)


-- | Sanitize variable names to be JavaScript-compatible identifiers.
-- Ordinary names only need their primes replaced. Operator names (and the
-- generated names derived from them, like <+>_arg1) contain characters that
-- are not legal in JavaScript identifiers; they are encoded under a "$op"
-- prefix with one mnemonic per character. '$'-only operators ($, $$) need no
-- encoding: '$' is a legal JavaScript identifier character, and the
-- uniqueness counter appended by 'unique' keeps them distinct from
-- compiler-internal '$'-prefixed names.
sanitizeForJS :: VarName -> VarName
sanitizeForJS v
  | any (`elem` jsHostileOpChars) v = "$op" ++ concatMap opCharCode v
  | otherwise                       = map sanitizeChar v
  where
    sanitizeChar '\'' = '_'  -- Replace single quotes with underscores
    sanitizeChar c = c        -- Keep other characters as-is
    opCharCode c = case c of
      '<' -> "$lt";   '>' -> "$gt";    '=' -> "$eq";    '+' -> "$plus"
      '-' -> "$minus";'*' -> "$star";  '/' -> "$slash"; '^' -> "$caret"
      '@' -> "$at";   '|' -> "$bar";   '&' -> "$amp";   '$' -> "$dollar"
      '%' -> "$pct";  '!' -> "$bang";  '~' -> "$tilde"; '?' -> "$quest"
      ':' -> "$colon";'.' -> "$dot";   '\'' -> "_"
      _   -> [c]

unique :: VarName -> S VarName
unique v = do
  n <- State.get
  put (n + 1)
  return $ sanitizeForJS v ++ show n


lookforalpha :: VarName -> Env -> VarName
lookforalpha v m = Map.findWithDefault v v m


lookforgen :: VarName -> Env -> S VarAccess
lookforgen v m =
    case Map.lookup v m of
       Just v -> return $ RegVar v
       Nothing -> do
          (unqualEnv, _) <- ask
          case Map.lookup v unqualEnv of
            Just lib' -> return $ LibVar lib' v
            -- An unresolved ordinary name falls through to the ambient
            -- builtins (BaseName). No builtin has an operator name, and the
            -- fallthrough would emit the invalid JavaScript rt.<+>, so an
            -- unresolved operator is an error here.
            Nothing
              | isOperatorName v ->
                  lift $ throwError $
                    "unbound operator '" ++ v ++ "': it is neither defined"
                    ++ " in this file nor imported unqualified"
              | otherwise -> return $ BaseName v


extend :: VarName -> VarName -> Env -> Env
extend v v' m = Map.insert v v' m

-- | Rename a Located Term, preserving the location
rename :: LTerm -> Env -> S LTerm
rename (Loc pos term) m = do
  term' <- renameTerm pos term m
  return $ Loc pos term'

-- | Rename the inner Term given its position (for qualified access resolution)
renameTerm :: PosInf -> Core.Term -> Env -> S Core.Term
renameTerm _ (Lit l) _ = return (Lit l)
renameTerm _ (Error t) m = do
      t' <- rename t m
      return $ Error t'
renameTerm pos (Var (RegVar v)) m = do
  v' <- lookforgen v m
  return $ Var v'

renameTerm _ (Var x) _  = return $ Var x
renameTerm _ (Abs l) m = do
  l' <- renameLambda l m
  return $ Abs l'
renameTerm _ (App t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ App t1' t2'
renameTerm _ (Let decl t) m = do
  (m', decl') <- renameDecl decl m
  t' <- rename t m'
  return $ Let decl' t'

renameTerm _ (If t1 t2 t3) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  t3' <- rename t3 m
  return $ If t1' t2' t3'

renameTerm _ (AssertElseError t1 t2 t3) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  t3' <- rename t3 m
  return $ AssertElseError t1' t2' t3'


renameTerm _ (Tuple terms tag) m = do
  terms' <- mapM (flip rename m) terms
  return $ Tuple terms' tag

renameTerm _ (Record fields) m = do
  fields' <- mapM renameField fields
  return $ Record fields'
     where renameField (f, t) = do
                   t' <- rename t m
                   return (f, t')

renameTerm _ (WithRecord e fields) m = do
  t' <- rename e m
  fs <- mapM renameField fields
  return $ WithRecord t' fs
  where renameField (f, t) = do
                   t' <- rename t m
                   return (f, t')

renameTerm pos (ProjField lt f) m = do
  maybeQualified <- tryQualifiedAccess
  case maybeQualified of
    Just term -> return term
    Nothing   -> do
      lt' <- rename lt m
      return $ ProjField lt' f
  where
    tryQualifiedAccess = case lt of
      -- Check if this is a qualified module access (e.g., A.foo or Alias.foo)
      -- At this stage, vars are RegVar from lowering, so we check RegVar
      Loc _ (Var (RegVar v)) | not (Map.member v m) -> do
        (_, libExports) <- ask
        case Map.lookup (LibName v) libExports of
          Just (originalLib, exports) ->
            if Set.member f exports
            then return $ Just (Var (LibVar originalLib f))  -- Use original lib for codegen
            else lift $ throwError $
              "Library '" ++ v ++ "' does not export '" ++ f ++ "'"
          Nothing -> return Nothing  -- Not a library access
      _ -> return Nothing
renameTerm _ (ProjIdx t idx) m = do
  t' <- rename t m
  return $ ProjIdx t' idx
renameTerm _ (List terms) m = do
  terms' <- mapM (flip rename m) terms
  return $ List terms'
renameTerm _ (ListCons t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ ListCons t1' t2'
renameTerm _ (Bin op t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ Bin op t1' t2'
renameTerm _ (Un op e) m = do
  e' <- rename e m
  return $ Un op e'

renameLambda :: Core.Lambda -> Env -> S Core.Lambda
renameLambda (Unary lv@(Loc vpos v) t) m = do
  v' <- unique v
  t' <- rename t $ extend v v' m
  return $ Unary (Loc vpos v') t'
renameLambda (Nullary t) m = do
  t' <- rename t m
  return $ Nullary t'


renameDecl :: Decl -> (Map.Map VarName VarName) -> S (Map.Map VarName VarName, Decl)
renameDecl (ValDecl v t) m = do
  v' <- unique v
  let m' = extend v v' m
  t' <- rename t m
  let decl' = (ValDecl v' t')
  return (m', decl')

renameDecl (FunDecs decs) m = do
  m' <- foldM ext_funDecl m decs
  decs' <- mapM (\(FunDecl v l pos) -> liftM (\l' -> FunDecl (lookforalpha v m') l' pos) (renameLambda l m')) decs
  let decl' = (FunDecs decs')
  return (m', decl')
  where ext_funDecl m' (FunDecl v _ _) = do
          v' <- unique v
          return $ extend v v' m'



--------------------------------------------------
-- 3. Pretty printing
--------------------------------------------------


-- show is defined via pretty printing
instance Show Term
  where show t = PP.render (runPPDefault (ppTermInner 0 t))

instance ShowIndent Prog where
  showIndent k t = PP.render (nest k (runPPDefault (ppProg t)))

instance ShowDebug Prog where
  showDebugWith cfg = PP.render . runPP cfg . ppProg
--------------------------------------------------




ppProg :: Prog -> PP PP.Doc
ppProg (Prog (Imports imports) term) = do
  termDoc <- ppLTerm 0 term
  let ppImports = if null imports then PP.empty else text "<<imports>>\n"
  pure $ ppImports $$ termDoc

-- | Pretty print a Located Term
ppLTerm :: Precedence -> LTerm -> PP PP.Doc
ppLTerm parentPrec = ppLocated (ppTermInner parentPrec)

-- | Pretty print a Term (inner, without location)
ppTermInner :: Precedence -> Term -> PP PP.Doc
ppTermInner parentPrec t = do
   let thisTermPrec = termPrec t
   doc <- ppTerm' t
   pure $ PP.maybeParens (thisTermPrec < parentPrec) doc

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppTerm' :: Term -> PP PP.Doc
ppTerm' (Lit literal) = pure $ ppLit literal

ppTerm' (Error lt) = do
  d <- ppLTerm 0 lt
  pure $ text "error " PP.<> d

ppTerm' (Tuple lts _) = do
  ds <- mapM (ppLTerm 0) lts
  pure $ PP.parens $ PP.hcat $ PP.punctuate (text ",") ds

ppTerm' (List lts) = do
  ds <- mapM (ppLTerm 0) lts
  pure $ PP.brackets $ PP.hcat $ PP.punctuate (text ",") ds

ppTerm' (Record fs) = do
  fDoc <- qqLFields fs
  pure $ PP.braces fDoc

ppTerm' (WithRecord le fs) = do
  leDoc <- ppLTerm 0 le
  fsDoc <- qqLFields fs
  pure $ PP.braces $ PP.hsep [leDoc, text "with", fsDoc]

ppTerm' (ProjField lt fn) = do
  d <- ppLTerm projPrec lt
  pure $ d PP.<> text "." PP.<> PP.text fn

ppTerm' (ProjIdx lt idx) = do
  d <- ppLTerm projPrec lt
  pure $ d PP.<> text "." PP.<> PP.text (show idx)


ppTerm' (ListCons lhd ltl) = do
  hdDoc <- ppLTerm consPrec lhd
  tlDoc <- ppLTerm consPrec ltl
  pure $ hdDoc PP.<> text "::" PP.<> tlDoc

ppTerm' (Var (RegVar x)) = pure $ text x
ppTerm' (Var (LibVar (LibName lib) var)) = pure $ text lib <+> text "." <+> text var
ppTerm' (Var (BaseName v)) = pure $ text v
ppTerm' (Abs lam) = do
  (ppArgs, ppBody) <- qqLambda lam
  pure $ text "fn" <+> ppArgs <+> text "=>" <+> ppBody

ppTerm' (App lt1 lt2) = do
  d1 <- ppLTerm appPrec lt1
  d2 <- ppLTerm argPrec lt2
  pure $ d1 <+> d2

ppTerm' (Let dec lbody) = do
  decDoc <- ppDecl dec
  bodyDoc <- ppLTerm 0 lbody
  pure $ text "let" <+>
    nest 3 decDoc $$
    text "in" <+>
    nest 3 bodyDoc $$
    text "end"


ppTerm' (If le0 le1 le2) = do
  d0 <- ppLTerm 0 le0
  d1 <- ppLTerm 0 le1
  d2 <- ppLTerm 0 le2
  pure $ text "if" <+>
    d0 $$
    text "then" <+>
    d1 $$
    text "else" <+>
    d2

ppTerm' (AssertElseError le0 le1 le2) = do
  d0 <- ppLTerm 0 le0
  d1 <- ppLTerm 0 le1
  d2 <- ppLTerm 0 le2
  pure $ text "assert" <+>
    d0 $$
    text "then" <+>
    d1 $$
    text "elseError" <+>
    d2



ppTerm' (Bin op lt1 lt2) = do
  let binOpPrec = opPrec op
  d1 <- ppLTerm binOpPrec lt1
  d2 <- ppLTerm binOpPrec lt2
  pure $ d1 <+> text (show op) <+> d2

ppTerm' (Un op lt) = do
  let unOpPrec = op1Prec op
  d <- ppLTerm unOpPrec lt
  pure $ text (show op) <+> d


-- | Pretty print LFields (fields with Located terms)
qqLFields :: LFields -> PP PP.Doc
qqLFields fs = do
  fieldDocs <- mapM ppField fs
  pure $ PP.hcat $ PP.punctuate (text ",") fieldDocs
     where ppField (name, lt) = do
              d <- ppLTerm 0 lt
              pure $ PP.hcat [PP.text name, PP.text "=", d]

qqLambda :: Lambda -> PP (PP.Doc, PP.Doc)
qqLambda (Unary (Loc _ arg) lbody) = do
  bodyDoc <- ppLTerm 0 lbody
  pure (text arg, bodyDoc)
qqLambda (Nullary lbody) = do
  bodyDoc <- ppLTerm 0 lbody
  pure (text "()", bodyDoc)

ppDecl :: Decl -> PP PP.Doc
ppDecl (ValDecl arg lt) = do
  d <- ppLTerm 0 lt
  pure $ text "val" <+> text arg <+> text "=" <+> d
ppDecl (FunDecs fs) = ppFuns fs
  where
    ppFunDecl prefix (FunDecl fname lam _) =
      ppFunOptions (prefix ++ " " ++ fname) lam

    ppFunOptions prefix lam = do
        (ppArgs, ppBody) <- qqLambda lam
        pure $ text prefix <+> ppArgs <+> text "=" <+> nest 2 ppBody


    ppFuns (doc:docs) = do
      let ppFirstFun = ppFunDecl "fun"
          ppOtherFun = ppFunDecl "and"
      firstDoc <- ppFirstFun doc
      otherDocs <- mapM ppOtherFun docs
      pure $ firstDoc $$ vcat otherDocs


    ppFuns _ = pure PP.empty


ppLit :: Lit -> PP.Doc
ppLit (LNumeric (NumInt i))  = PP.integer i
ppLit (LNumeric (NumFloat f)) = PP.double f
ppLit (LString s)   = PP.doubleQuotes (text s)
ppLit (LLabel s)    = PP.braces (text s)
ppLit LUnit         = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LDCLabel dc) = ppDCLabelExpLit dc


termPrec :: Term -> Precedence
termPrec (Lit _)           = maxPrec
termPrec (Tuple _ _)       = maxPrec
termPrec (List _)          = maxPrec
termPrec (Var _)           = maxPrec
termPrec (App _ _)         = appPrec
termPrec (Bin op _ _)      = opPrec op
termPrec (ListCons _ _)    = 200
termPrec _                 = 0


------------------------------------------------------------
-- s-expression serialization (see "Sexp")
------------------------------------------------------------

instance ToSexp Lit where
  toSexp (LNumeric (NumInt i))   = Lst [Atom "int", toSexp i]
  toSexp (LNumeric (NumFloat d)) = Lst [Atom "float", toSexp d]
  toSexp (LString s)             = Lst [Atom "string", Str s]
  toSexp (LBool b)               = Lst [Atom "bool", Atom (if b then "true" else "false")]
  toSexp LUnit                   = Atom "unit"
  toSexp (LLabel s)              = Lst [Atom "label-string", Str s]
  toSexp (LDCLabel dc)           = toSexp dc

instance FromSexp Lit where
  fromSexp (Lst [Atom "int", nD])    = (LNumeric . NumInt) <$> fromSexp nD
  fromSexp (Lst [Atom "float", nD])  = (LNumeric . NumFloat) <$> fromSexp nD
  fromSexp (Lst [Atom "string", sD]) = LString <$> asName sD
  fromSexp (Lst [Atom "bool", bD])   = do
    b <- asToken bD
    case b of
      "true"  -> Right (LBool True)
      "false" -> Right (LBool False)
      _       -> Left ("bad boolean literal: " ++ b)
  fromSexp (Atom "unit")                    = Right LUnit
  fromSexp (Lst [Atom "label-string", sD])  = LLabel <$> asName sD
  fromSexp d@(Lst (Atom "dclabel" : _))     = LDCLabel <$> fromSexp d
  fromSexp d = Left ("not a valid literal, got " ++ headHint d)
