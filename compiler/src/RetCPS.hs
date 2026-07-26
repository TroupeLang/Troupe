{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module RetCPS
  ( VarName(..)
  , LVarName
  , KLambda(..)
  , SVal(..)
  , ContDef(..)
  , FunDef(..)
  , Fields
  , LFields
  , SimpleTerm(..)
  , KTerm(..)
  , Prog(..)
  , LKTerm
  , LSimpleTerm
  , ppKTerm
  , ppSimpleTerm
  )
where

import GHC.Generics
import qualified Data.Serialize as Serialize

import Basics(BinOp(..),UnaryOp(..),Precedence)
import qualified Basics
import qualified Core as C
import Core (ppLit)
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, vcat, nest)
import           ShowIndent

import TroupePositionInfo (Located(..), noLoc)
import PrettyPrint (PP, runPP, runPPDefault, ppLocated, ShowDebug(..))

newtype VarName = VN Basics.VarName
    deriving (Eq, Ord, Generic)


instance Serialize.Serialize VarName
instance Show VarName where
  show (VN x) = show x

-- newtype KontName = K Basics.VarName deriving (Eq,Ord)
-- instance Show KontName where
--  show (K k) = "K" ++ k


{--

The language here is only the "surface-level" CPS, i.e., it does not contain any
runtime-specific terms; this also means that if we were to code gen we would be
doing that from this language

 -}

-- Located type aliases
type LKTerm = Located KTerm
type LSimpleTerm = Located SimpleTerm
type LVarName = Located VarName

data KLambda = Unary LVarName LKTerm
             | Nullary LKTerm
  deriving (Eq, Ord, Show)

data SVal
   = KAbs KLambda
   | Lit C.Lit
     deriving (Eq, Ord, Show)

data ContDef = Cont VarName LKTerm
               deriving (Eq, Ord)
data FunDef = Fun VarName KLambda   -- Position is on the Located wrapper when used
              deriving (Eq, Ord)

-- Fields without location info (for backward compatibility in some contexts)
type Fields = [(Basics.FieldName, VarName)]
-- Fields with location info for variable references
type LFields = [(Basics.FieldName, LVarName)]

data SimpleTerm
   = Bin BinOp LVarName LVarName
   | Un UnaryOp LVarName
   | ValSimpleTerm SVal
   | Tuple [LVarName] Basics.SynVariantTag
   | Record LFields
   | WithRecord LVarName LFields
   | ProjField LVarName Basics.FieldName
   | ProjIdx LVarName Word
   | List [LVarName]
   | ListCons LVarName LVarName
   | Base Basics.VarName
   | Lib Basics.LibName Basics.VarName
     deriving (Eq, Ord, Show)

-- | KTerm represents continuation-passing style terms.
-- Error and AssertElseError no longer use ErrorPosInf - position comes from Located wrapper.
data KTerm
    = LetSimple VarName LSimpleTerm LKTerm
    | LetFun [(Located FunDef)] LKTerm
    | LetRet ContDef LKTerm
    | KontReturn VarName
    | ApplyFun VarName VarName
    | If VarName LKTerm LKTerm
    | AssertElseError VarName LKTerm VarName  -- Position from Located wrapper
    | Error VarName                            -- Position from Located wrapper
    | Halt VarName
    -- ; aa; 2018-07-02; bringing Halt back because
    -- of exports

      deriving (Eq, Ord)

data Prog = Prog LKTerm
  deriving (Eq, Show)

-- GetPosInfo instances are now provided by the Located wrapper
-- via TroupePositionInfo's instance: GetPosInfo (Located a)

--------------------------------------------------
-- show is defined via pretty printing
instance Show KTerm
  where show t = PP.render (runPPDefault (ppKTerm 0 (noLoc t)))

instance Show ContDef
  where show (Cont x t) = PP.render (runPPDefault (ppKTerm 0 t))
instance ShowIndent Prog where
  showIndent k p = PP.render (nest k (runPPDefault (ppProg p)))

instance ShowDebug Prog where
  showDebugWith cfg = PP.render . runPP cfg . ppProg
--------------------------------------------------
-- obs: these functions are not exported
--

ppProg :: Prog -> PP PP.Doc
ppProg (Prog lkterm) = do
  ktDoc <- ppKTerm 0 lkterm
  pure ktDoc

ppKTerm :: Precedence -> LKTerm -> PP PP.Doc
ppKTerm parentPrec = ppLocated (ppKTermInner parentPrec)

ppKTermInner :: Precedence -> KTerm -> PP PP.Doc
ppKTermInner parentPrec t =
   let thisTermPrec = 1000
   in do
     doc <- ppKTerm' t
     pure $ PP.maybeParens (thisTermPrec < parentPrec) doc

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm'       Core.LAtom _ -> Nothingt

-- ppLit :: C.Lit -> PP.Doc
-- ppLit = C.ppLit
-- ppLit  (C.LInt i pi) = PP.integer i
-- ppLit  (C.LString s)   = PP.doubleQuotes (text s)
-- ppLit  (C.LLabel s)    = PP.braces (text s)
-- ppLit  (C.LUnit) = text "()"
-- ppLit  (C.LBool True) = text "true"
-- ppLit  (C.LBool False) = text "false"
-- ppLit  (C.LAtom a) = text a

textv (VN x) = text x

-- Pretty print a Located VarName (extracts the VarName)
textlv :: LVarName -> PP PP.Doc
textlv = ppLocated (pure . textv)

ppSimpleTerm :: SimpleTerm -> PP PP.Doc
ppSimpleTerm (Bin op lv1 lv2) = do
  d1 <- textlv lv1
  d2 <- textlv lv2
  pure $ d1 <+> text (show op) <+> d2
ppSimpleTerm (Un op lv) = do
  d <- textlv lv
  pure $ text (show op) <+> d
ppSimpleTerm (ValSimpleTerm (Lit lit)) =
  pure $ ppLit lit
ppSimpleTerm (ValSimpleTerm (KAbs klam)) =
  ppKLambda klam
ppSimpleTerm (Tuple vars _) = do
  ds <- mapM textlv vars
  pure $ PP.parens $ PP.hsep $ PP.punctuate (text ",") ds
ppSimpleTerm (List vars) = do
  ds <- mapM textlv vars
  pure $ PP.brackets $ PP.hsep $ PP.punctuate (text ",") ds
ppSimpleTerm (ListCons v1 v2) = do
  d1 <- textlv v1
  d2 <- textlv v2
  pure $ PP.parens $ d1 PP.<> text "::" PP.<> d2
ppSimpleTerm (Base b) = pure $ text b PP.<> text "$base"
ppSimpleTerm (Lib (Basics.LibName lib) v) = pure $ text lib <+> text "." <+> text v
ppSimpleTerm (Record fields) = do
  fDoc <- qqLFields fields
  pure $ PP.braces fDoc
ppSimpleTerm (WithRecord x fields) = do
  xDoc <- textlv x
  fDoc <- qqLFields fields
  pure $ PP.braces $ PP.hsep [xDoc, text "with", fDoc]

ppSimpleTerm (ProjField x f) = do
  d <- textlv x
  pure $ d PP.<> text "." PP.<> PP.text f
ppSimpleTerm (ProjIdx x idx) = do
  d <- textlv x
  pure $ d PP.<> text "." PP.<> PP.text (show idx)

-- Pretty print LFields (fields with Located VarNames)
qqLFields :: LFields -> PP PP.Doc
qqLFields fields = do
  fieldDocs <- mapM ppField fields
  pure $ PP.hcat $ PP.punctuate (text ",") fieldDocs
    where ppField (name, lv) = do
            lvDoc <- textlv lv
            pure $ PP.hcat [PP.text name, PP.text "=", lvDoc]


ppKLambda :: KLambda -> PP PP.Doc
ppKLambda (Unary (Loc _ pat) kt) = do
  ktDoc <- ppKTerm 0 kt
  pure $ text "fn" <+> textv pat <+> text "=>" <+> ktDoc
ppKLambda (Nullary kt) = do
  ktDoc <- ppKTerm 0 kt
  pure $ text "fn" <+> text "()" <+> text "=>" <+> ktDoc

ppKTerm' :: KTerm -> PP PP.Doc
ppKTerm' (Error v) = pure $ text "error" PP.<> textv v
-- ppKTerm'  (Abs lam) = ppLambda lam

--ppKTerm' (ApplyKont kname varname) =
--    text (show kname) <+> textv varname

ppKTerm' (Halt varname) =
  pure $ text "halt" <+> textv varname

ppKTerm' (KontReturn varname) =
  pure $ text "return" <+> textv varname

-- ppKTerm' (LetRet kname kterm) =
--   text "let-ret" <+> (text (show kname)) $$
--   text "in" <+>
--   nest 3 (ppKTerm 0 kterm) $$
--   text "end"

ppKTerm' (ApplyFun fname varname) =
    pure $ textv fname <+> textv varname

ppKTerm' (LetSimple x (Loc _ t) k) = do
  tDoc <- ppSimpleTerm t
  kDoc <- ppKTerm 0 k
  pure $ text "let-simple" <+>
    nest 3 (textv x <+> text "=" <+> tDoc) $$
    text "in" <+>
    nest 3 kDoc $$
    text "end"

ppKTerm' (LetRet (Cont pat kt1) kt2) = do
  kt1Doc <- ppKTerm 0 kt1
  kt2Doc <- ppKTerm 0 kt2
  pure $ text "let-ret" <+>
    nest 3 (textv pat <+> text "=" <+> kt1Doc) $$
    text "in" <+>
    nest 3 kt2Doc $$
    text "end"

ppKTerm' (LetFun lfdefs kt) = do
  funsDoc <- ppFuns =<< mapM ppFunDecl lfdefs
  ktDoc <- ppKTerm 0 kt
  pure $ text "let-fun" <+>
    nest 3 funsDoc $$
    text "in" <+>
    nest 3 ktDoc $$
    text "end"
  where
    ppFunDecl (Loc _ (Fun fname (Unary (Loc _ pat) body))) = do
       bodyDoc <- ppKTerm 0 body
       pure (textv fname <+> textv pat <+> text "=", bodyDoc)
    ppFunDecl (Loc _ (Fun fname (Nullary body))) = do
       bodyDoc <- ppKTerm 0 body
       pure (textv fname <+> text "()" <+> text "=", bodyDoc)
    ppFuns (doc:docs) =
      let pp' prefix (docHead, docBody) = text prefix <+> docHead $$ nest 2 docBody
          ppFirstFun = pp' "fun"
          ppOtherFun = pp' "and"
      in pure $ ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = pure PP.empty

ppKTerm' (If vname kt1 kt2) = do
  kt1Doc <- ppKTerm 0 kt1
  kt2Doc <- ppKTerm 0 kt2
  pure $ text "if" <+>
    textv vname $$
    text "then" <+>
    kt1Doc $$
    text "else" <+>
    kt2Doc

ppKTerm' (AssertElseError vname kt1 verr) = do
  kt1Doc <- ppKTerm 0 kt1
  pure $ text "assert" <+>
    textv vname $$
    text "then" <+>
    kt1Doc $$
    text "elseError" <+>
    textv verr