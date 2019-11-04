{-# LANGUAGE DeriveGeneric #-}

module IRANF
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

newtype VarName = VN Basics.VarName
    deriving (Eq, Ord, Generic)

instance Serialize.Serialize VarName
instance Show VarName where
  show (VN x) = show x

data LambdaExp = Unary VarName ATerm
    deriving (Eq)

data FunDef = Fun VarName LambdaExp
              deriving (Eq)

data SVal
  = KAbs LambdaExp
  | Lit C.Lit
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


data ATerm 
    = Ret VarName 
    | TailApply VarName VarName 
    | Call VarName ATerm ATerm 
    | Let VarName SimpleTerm ATerm 
    | LetFun [FunDef] ATerm
    | If VarName ATerm ATerm
    | AssertElseError VarName ATerm VarName
    | Error VarName -- TODO: perhaps it may be better if this is a string and not a varname(?) ; 2019-03-21; AA
    deriving (Eq)

data Prog = Prog C.Atoms ATerm
  deriving (Eq, Show)

instance Show ATerm  
  where show t = PP.render (ppATerm 0 t)

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
  in ppAtoms $$ ppATerm 0 kterm

ppATerm :: Precedence -> ATerm -> PP.Doc

ppATerm parentPrec t =
   let thisTermPrec = 1000
   in PP.maybeParens (thisTermPrec < parentPrec   )  $ ppATerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppLit :: C.Lit -> PP.Doc
ppLit  (C.LInt i) = PP.integer i
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


ppKLambda :: LambdaExp -> PP.Doc
ppKLambda (Unary v kt) =
  text "fn" <+>  textv v <+> text "=>" <+> ppATerm 0 kt


ppATerm' :: ATerm -> PP.Doc
ppATerm'  (Error v) = text "error" PP.<> textv v


ppATerm' (Ret varname) =
  text "return" <+> textv varname


ppATerm' (TailApply fname varname) =
    textv fname <+> textv varname

ppATerm' (Let x t k) =
  text "let-simple" <+>
  nest 3 (textv x <+> text "=" <+> ppSimpleTerm t) $$
  text "in" <+>
  nest 3 (ppATerm 0 k) $$
  text "end"

ppATerm' (Call x t1 t2) =
    text "let-call" <+>
    nest 3 (textv x <+> text "=" <+> ppATerm' t1) $$
    text "in" <+>
    nest 3 (ppATerm 0 t2) $$
    text "end"


ppATerm' (LetFun fdefs kt) =
  text "let-fun" <+>
  nest 3 (ppFuns (map ppFunDecl fdefs)) $$
  text "in" <+>
  nest 3 (ppATerm 0 kt) $$
  text "end"
  where
    ppFunDecl (Fun fname (Unary pat body)) =
       (textv fname  <+> textv pat <+> text "=" , ppATerm 0 body)
    ppFuns (doc:docs) =
      let pp' prefix (docHead,docBody) = text prefix  <+> docHead  $$ nest 2 docBody
          ppFirstFun = pp' "fun"
          ppOtherFun = pp' "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = PP.empty

    
ppATerm' (If vname kt1 kt2) =
  text "if" <+>
  textv vname $$
  text "then" <+>
  ppATerm 0 kt1 $$
  text "else" <+>
  ppATerm 0 kt2



ppATerm' (AssertElseError vname kt1 verr) =
  text "assert" <+>
  textv vname $$
  text "then" <+>
  ppATerm 0 kt1 $$
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

termPrec :: ATerm -> Precedence
termPrec (Ret _)    = appPrec
termPrec (TailApply _ _) = appPrec
termPrec (Call _ _ _) = 0
termPrec (Let _ _ _) = 0
termPrec (LetFun    _ _)   = 0
termPrec (If _ _ _)        = 0
termPrec (Error _ ) = 0

