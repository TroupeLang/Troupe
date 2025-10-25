module DirectWOPats ( Lambda (..)
              , Term (..)
              , Decl (..)
              , FunDecl (..)
              , Lit(..)
              , SyntacticVariantName
              , SyntacticVariantConstructor
              , SyntacticVariants(..)
              , Prog(..)            
              )
where

import Basics
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import ShowIndent
import DCLabels
import TroupePositionInfo
import Data.List (find)

data Decl
    = ValDecl VarName Term
    | FunDecs [FunDecl]
  deriving (Eq)

data FunDecl = FunDecl VarName Lambda
  deriving (Eq)

data Lit
    = LInt Integer PosInf
    | LString String
    | LLabel String
    | LDCLabel DCLabelExp
    | LUnit
    | LBool Bool
    | LSyntacticVariant SyntacticVariantName
  deriving (Eq, Show)



data Lambda = Lambda [VarName] Term
  deriving (Eq)

type Fields = [(FieldName, Term)]

data Term
    = Lit Lit
    | Var VarName
    | Abs Lambda
    | App Term [Term]
    | Let [Decl] Term    
    | If Term Term Term
    | AssertElseError Term Term Term PosInf
    | Tuple [Term] SynVariantTag
    | Record Fields
    | WithRecord Term Fields
    | ProjField Term FieldName 
    | ProjIdx Term Word
    | List [Term]
    | ListCons Term Term
    | Bin BinOp Term Term
    | Un UnaryOp Term
    | Error Term PosInf
    deriving (Eq)

data SyntacticVariants = SyntacticVariants [SyntacticVariantDef]
      deriving (Eq, Show)

data Prog = Prog Imports SyntacticVariants Term
  deriving (Eq, Show)


--------------------------------------------------
-- show is defined via pretty printing
instance Show Term
  where show t = PP.render (ppTerm 0 t)

instance ShowIndent Prog where
  showIndent k t = PP.render (nest k (ppProg t))
--------------------------------------------------
-- obs: these functions are not exported
--




ppProg :: Prog -> PP.Doc
ppProg (Prog (Imports imports) (SyntacticVariants datatypes) term) =
  let ppSyntacticVariants =
        if null datatypes
        then PP.empty
        else vcat $ flip map datatypes (\dt -> (text "datatype ") <+>
                                              (text $ fst dt) <+>
                                              (hsep $ PP.punctuate (text " |") (map ppConstructor $ snd dt)))
        where ppConstructor (s, []) = text s
              ppConstructor (s, x:[]) = text s <+> text " of " <+> text x
              ppConstructor (s, xs) = text s <+> text " of " <+> PP.parens (hsep $ PP.punctuate (text " *") (map text xs))
      ppImports = if null imports then PP.empty else text "<<imports>>\n"
  in ppImports $$ ppSyntacticVariants $$ ppTerm 0 term


ppTerm :: Precedence -> Term -> PP.Doc
ppTerm parentPrec t =
   let thisTermPrec = termPrec t
   in PP.maybeParens (thisTermPrec < parentPrec )
      $ ppTerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppTerm' :: Term -> PP.Doc
ppTerm' (Lit literal) = ppLit literal

ppTerm' (Error t _) = text "error " PP.<> ppTerm' t

ppTerm'  (Tuple ts False) =
  PP.parens $
  PP.hcat $
  PP.punctuate (text ",") (map (ppTerm 0) ts)
ppTerm' (Tuple ts True) =
  case ts of [Lit (LString nm)] -> text nm
             [Lit (LString nm), t] -> text nm PP.<> PP.space PP.<> ppTerm 0 t
             otherwise -> text "error: Missing syntactiv variant"

ppTerm' (Record fs) = PP.braces $  qqFields fs

ppTerm' (WithRecord e fs) =
    PP.braces $ PP.hsep [ ppTerm 0 e, text "with", qqFields fs ]

ppTerm' (ProjField t fn) =
  ppTerm projPrec t PP.<> text "." PP.<> PP.text fn

ppTerm' (ProjIdx t idx) =
  ppTerm projPrec t PP.<> text "." PP.<> PP.text (show idx)


ppTerm'  (List ts) =
  PP.brackets $
  PP.hcat $
  PP.punctuate (text ",") (map (ppTerm 0) ts)



ppTerm' (ListCons hd tl) =
   ppTerm consPrec hd PP.<> text "::" PP.<> ppTerm consPrec tl

ppTerm' (Var x) = text x
ppTerm' (Abs lam) =
  let (ppArgs, ppBody) = qqLambda lam
  in text "fn" <+> ppArgs <+> text "=>" <+> ppBody

ppTerm' (App t1 t2s) =
    ppTerm appPrec t1
          <+> (hsep (map (ppTerm argPrec) t2s))

ppTerm' (Let decs body) =
  text "let" <+>
  nest 3 (vcat (map ppDecl decs)) $$
  text "in" <+>
  nest 3 (ppTerm 0 body) $$
  text "end"


ppTerm' (If e0 e1 e2) =
  text "if" <+>
  ppTerm 0 e0 $$
  text "then" <+>
  ppTerm 0 e1 $$
  text "else" <+>
  ppTerm 0 e2

ppTerm' (AssertElseError e0 e1 e2 _) =
  text "assert" <+>
  ppTerm 0 e0 $$
  text "then" <+>
  ppTerm 0 e1 $$
  text "elseError" <+>
  ppTerm 0 e2
  

ppTerm' (Bin op t1 t2) =
  let binOpPrec = opPrec op
  in
     ppTerm binOpPrec t1 <+>
     text (show op) <+>
     ppTerm binOpPrec t2

ppTerm' (Un op t) =
  let unOpPrec = op1Prec op
  in
     text (show op) <+>
     ppTerm unOpPrec t



qqFields fs = PP.hcat $
    PP.punctuate (text ",") (map ppField fs)
     where ppField (name, t)  = 
              PP.hcat [PP.text name, PP.text "=", ppTerm 0 t ]


qqLambda :: Lambda -> (PP.Doc, PP.Doc)
qqLambda (Lambda args body) =
  let ppArgs' =
        if null args then text "()"
                     else hsep $ map text args
  in ( ppArgs', ppTerm 0 body)

ppDecl (ValDecl x t) = text "val" <+> text x <+> text "=" <+> ppTerm 0 t
ppDecl (FunDecs fs) = ppFuns (map ppFunDecl fs)
  where
    ppFunDecl ( FunDecl fname (Lambda args body)) =
      let ppArgs = if args == [] then text "()" else hsep ( map text args)
      in (text fname <+> ppArgs <+> text "=" , ppTerm 0 body)
    ppFuns (doc:docs) =
      let pp' prefix (docHead,docBody) = text prefix  <+> docHead  $$ nest 2 docBody
          ppFirstFun = pp' "fun"
          ppOtherFun = pp' "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = PP.empty

ppLit :: Lit -> PP.Doc
ppLit (LInt i _)      = PP.integer i
ppLit (LString s)   = PP.doubleQuotes (text s)
ppLit (LLabel s)    = PP.braces (text s)
ppLit (LDCLabel dc) = ppDCLabelExpLit dc
ppLit LUnit         = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LSyntacticVariant a) = text a


termPrec :: Term -> Precedence
termPrec (Lit _)         = maxPrec
termPrec (Tuple _ _)       = maxPrec
termPrec (List _ )       = maxPrec
termPrec (Var _)         = maxPrec
termPrec (App _ _)       = appPrec
termPrec (Bin op _ _)    = opPrec op
termPrec (ListCons _ _)  = 200
termPrec _               = 0
