module DirectWOPats ( Lambda (..)
              , Term (..)
              , Decl (..)
              , FunDecl (..)
              , Lit(..)
              , AtomName
              , Atoms(..)
              , Prog(..)
              )
where

import Basics
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import ShowIndent
import TroupePositionInfo

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
    | LUnit
    | LBool Bool
    | LAtom AtomName
  deriving (Eq, Show)



data Lambda = Lambda [VarName] Term
  deriving (Eq)

data Term
    = Lit Lit
    | Var VarName
    | Abs Lambda
    | App Term [Term]
    | Let [Decl] Term
    -- | Case Term [(VarName, Term)]
    | If Term Term Term
    | AssertElseError Term Term Term PosInf
    | Tuple [Term]
    | List [Term]
    | ListCons Term Term
    | Bin BinOp Term Term
    | Un UnaryOp Term
    | Error Term PosInf
    deriving (Eq)

data Atoms = Atoms [AtomName]
      deriving (Eq, Show)

data Prog = Prog Imports Atoms Term
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
type Precedence = Integer




ppProg :: Prog -> PP.Doc
ppProg (Prog (Imports imports) (Atoms atoms) term) =
  let ppAtoms =
        if null atoms
          then PP.empty
          else (text "datatype Atoms = ") <+>
               (hsep $ PP.punctuate (text " |") (map text atoms))
      ppImports = if null imports then PP.empty else text "<<imports>>\n"
  in ppImports $$ ppAtoms $$ ppTerm 0 term


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

ppTerm'  (Tuple ts) =
  PP.parens $
  PP.hcat $
  PP.punctuate (text ",") (map (ppTerm 0) ts)

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

{--
ppTerm' (Case e cases) =
  text "case" <+>
  ppTerm 0 e  $$
  nest 2 (ppCases cases)
  where
    ppCases [] = error "empty cases"
    ppCases (first:rest) =
      text "of" <+> ppCaseBody first $$
      vcat (map ppNonFirst rest)

    ppNonFirst second =
      text " |" <+> ppCaseBody second

    ppCaseBody (decl, term) =
      text (show decl) <+> text "=>" <+> ppTerm 0 term

--}

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
ppLit LUnit         = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LAtom a) = text a


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
opPrec And   = 50
opPrec Index = 50
opPrec FlowsTo    = 50
opPrec RaisedTo   = 50

op1Prec :: UnaryOp -> Precedence
op1Prec x = 50

appPrec :: Precedence
appPrec = 5000

argPrec :: Precedence
argPrec = appPrec + 1

maxPrec :: Precedence
maxPrec = 100000

consPrec :: Precedence
consPrec = 6000

termPrec :: Term -> Precedence
termPrec (Lit _)         = maxPrec
termPrec (Tuple _)       = maxPrec
termPrec (List _ )       = maxPrec
termPrec (Var _)         = maxPrec
termPrec (App _ _)       = appPrec
termPrec (Bin op _ _)    = opPrec op
termPrec (ListCons _ _)  = 200
termPrec _               = 0
