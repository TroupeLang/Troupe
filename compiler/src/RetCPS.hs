{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module RetCPS
where

import GHC.Generics
import qualified Data.Serialize as Serialize

import Basics(BinOp(..),UnaryOp(..))

import qualified Basics
import qualified Core as C
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import           ShowIndent

import TroupePositionInfo

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

data KLambda = Unary VarName KTerm
             | Nullary KTerm
  deriving (Eq)

data SVal
   = KAbs KLambda
   | Lit C.Lit
     deriving (Eq)

data ContDef = Cont VarName KTerm
               deriving (Eq)
data FunDef = Fun VarName KLambda
              deriving (Eq)

data SimpleTerm
   = Bin BinOp VarName VarName
   | Un UnaryOp VarName
   | ValSimpleTerm SVal
   | Tuple [VarName]
   | List [VarName]
   | ListCons VarName VarName
   | Base Basics.VarName
   | Lib Basics.LibName Basics.VarName
     deriving (Eq)

data KTerm
    = LetSimple VarName SimpleTerm KTerm
    | LetFun [FunDef] KTerm
    | LetRet ContDef KTerm
    | KontReturn VarName
    | ApplyFun VarName VarName
    | If VarName KTerm KTerm
    | AssertElseError VarName KTerm VarName PosInf
    | Error VarName PosInf
    | Halt VarName
    -- ; aa; 2018-07-02; bringing Halt back because
    -- of exports

      deriving (Eq)

data Prog = Prog C.Atoms KTerm
  deriving (Eq, Show)

--------------------------------------------------
-- show is defined via pretty printing
instance Show KTerm
  where show t = PP.render (ppKTerm 0 t)

instance ShowIndent Prog where
  showIndent k p = PP.render (nest k (ppProg p))
--------------------------------------------------
-- obs: these functions are not exported
--
type Precedence = Integer

ppProg :: Prog -> PP.Doc
ppProg (Prog (C.Atoms atoms) kterm) =
  let ppAtoms =
        if null atoms
          then PP.empty
          else (text "datatype Atoms = ") <+>
               (hsep $ PP.punctuate (text " |") (map text atoms))
  in ppAtoms $$ ppKTerm 0 kterm

ppKTerm :: Precedence -> KTerm -> PP.Doc

ppKTerm parentPrec t =
   let thisTermPrec = 1000
   in PP.maybeParens (thisTermPrec < parentPrec   )  $ ppKTerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppLit :: C.Lit -> PP.Doc
ppLit  (C.LInt i pi) = PP.integer i 
ppLit  (C.LString s)   = PP.doubleQuotes (text s)
ppLit  (C.LLabel s)    = PP.braces (text s)
ppLit  (C.LUnit) = text "()"
ppLit  (C.LBool True) = text "true"
ppLit  (C.LBool False) = text "false"
ppLit  (C.LAtom a) = text a

textv (VN x) = text x

ppSimpleTerm :: SimpleTerm -> PP.Doc
ppSimpleTerm (Bin op (VN v1)  (VN v2)) =
  text v1 <+> text (show op) <+> text v2
ppSimpleTerm (Un op (VN v)) =
  text (show op) <+> text v
ppSimpleTerm (ValSimpleTerm (Lit lit)) =
  ppLit lit
ppSimpleTerm (ValSimpleTerm (KAbs klam)) =
  ppKLambda klam
ppSimpleTerm (Tuple vars) =
  PP.parens $ PP.hsep $ PP.punctuate (text ",") (map textv vars)
ppSimpleTerm (List vars) =
  PP.brackets $ PP.hsep $ PP.punctuate (text ",") (map textv vars)
ppSimpleTerm (ListCons v1 v2) =
  PP.parens $ textv v1 PP.<> text "::" PP.<> textv v2
ppSimpleTerm (Base b) = text b PP.<> text "$base"
ppSimpleTerm (Lib (Basics.LibName lib) v) = text lib <+> text "." <+> text v


ppKLambda :: KLambda -> PP.Doc
ppKLambda (Unary pat kt) =
  text "fn" <+>  textv pat <+> text "=>" <+> ppKTerm 0 kt
ppKLambda (Nullary kt) =
  text "fn" <+> text "()" <+> text "=>" <+> ppKTerm 0 kt

ppKTerm' :: KTerm -> PP.Doc
ppKTerm'  (Error v _) = text "error" PP.<> textv v
-- ppKTerm'  (Abs lam) = ppLambda lam

--ppKTerm' (ApplyKont kname varname) =
--    text (show kname) <+> textv varname

ppKTerm' (Halt varname) =
  text "halt" <+> textv varname

ppKTerm' (KontReturn varname) =
  text "return" <+> textv varname

-- ppKTerm' (LetRet kname kterm) =
--   text "let-ret" <+> (text (show kname)) $$
--   text "in" <+>
--   nest 3 (ppKTerm 0 kterm) $$
--   text "end"

ppKTerm' (ApplyFun fname varname) =
    textv fname <+> textv varname

ppKTerm' (LetSimple x t k) =
  text "let-simple" <+>
  nest 3 (textv x <+> text "=" <+> ppSimpleTerm t) $$
  text "in" <+>
  nest 3 (ppKTerm 0 k) $$
  text "end"

ppKTerm' (LetRet (Cont pat kt1) kt2) =
  text "let-ret" <+>
  nest 3 (textv pat <+> text "=" <+> ppKTerm' kt1) $$
  text "in" <+>
  nest 3 (ppKTerm 0 kt2) $$
  text "end"

ppKTerm' (LetFun fdefs kt) =
  text "let-fun" <+>
  nest 3 (ppFuns (map ppFunDecl fdefs)) $$
  text "in" <+>
  nest 3 (ppKTerm 0 kt) $$
  text "end"
  where
    ppFunDecl (Fun fname (Unary pat body)) =
       (textv fname  <+> textv pat <+> text "=" , ppKTerm 0 body)
    ppFunDecl (Fun fname (Nullary body)) =
       (textv fname  <+> text "()" <+> text "=" , ppKTerm 0 body)
    ppFuns (doc:docs) =
      let pp' prefix (docHead,docBody) = text prefix  <+> docHead  $$ nest 2 docBody
          ppFirstFun = pp' "fun"
          ppOtherFun = pp' "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = PP.empty

ppKTerm' (If vname kt1 kt2) =
  text "if" <+>
  textv vname $$
  text "then" <+>
  ppKTerm 0 kt1 $$
  text "else" <+>
  ppKTerm 0 kt2

ppKTerm' (AssertElseError vname kt1 verr _) =
  text "assert" <+>
  textv vname $$
  text "then" <+>
  ppKTerm 0 kt1 $$
  text "elseError" <+>
  textv verr



opPrec :: BinOp -> Precedence
opPrec Plus  = 100
opPrec Minus = 100
opPrec Mult  = 200
opPrec Div   = 200
opPrec Eq    = 50
opPrec Neq   = 50
opPrec Le    = 50
opPrec Lt    = 50
opPrec Ge    = 50
opPrec Gt    = 50

appPrec :: Precedence
appPrec = 5000

-- argPrec :: Precedence
-- argPrec = appPrec + 1

maxPrec :: Precedence
maxPrec = 100000

termPrec :: KTerm -> Precedence
termPrec (Halt _)       = maxPrec
termPrec (ApplyFun _ _) = appPrec
termPrec (KontReturn _)    = appPrec
termPrec (If _ _ _)        = 0
termPrec (LetSimple _ _ _) = 0
-- termPrec (LetCont   _ _)   = 0
termPrec (LetFun    _ _)   = 0
--termPrec (Case _ _)        = 0
