module Direct ( Lambda (..)
              , Term (..)
              , Decl (..)
              , FunDecl (..)
              , Lit(..)
              , DeclPattern(..)
              , AtomName
              , Atoms(..)
              , Prog(..)
              , Handler(..)
              )
where

import           Basics
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import           ShowIndent
import           TroupePositionInfo


data PrimType
    = TUnit
    | TInt
    | TBool
    | TString
  deriving (Eq, Ord, Show)

data Ty
    = TAny
    | TParam String
    | TPrim PrimType
    | TFun Ty [Ty]
    | TTuple [Ty]
    | TList Ty
  deriving (Eq)



data Lambda = Lambda [DeclPattern] Term --SrcPosInf
  deriving (Eq)

type Guard = Maybe Term
data Handler = Handler DeclPattern (Maybe DeclPattern) Guard Term
  deriving (Eq)


data DeclPattern
    = VarPattern VarName --SrcPosInf
    | ValPattern Lit 
    | AtPattern DeclPattern String --SrcPosInf
    | Wildcard --SrcPosInf
    | TuplePattern [DeclPattern] --SrcPosInf
    | ConsPattern DeclPattern DeclPattern --SrcPosInf
    | ListPattern [DeclPattern] --SrcPosInf
      deriving (Eq)

data Decl
    = ValDecl DeclPattern Term PosInf
    | FunDecs [FunDecl]
  deriving (Eq)

data FunDecl = FunDecl VarName [Lambda] PosInf
  deriving (Eq)

data Lit
    = LInt Integer PosInf
    | LUnit --SrcPosInf
    | LBool Bool --SrcPosInf
    | LString String --SrcPosInf
    | LLabel String --SrcPosInf
    | LAtom AtomName --SrcPosInf
  deriving (Eq, Show)

data Term
    = Lit Lit
    | Var VarName --SrcPosInf
    | Abs Lambda 
    | Hnd Handler
    | App Term [Term]
    | Let [Decl] Term
    | Case Term [(DeclPattern, Term)] PosInf
    | If Term Term Term
    | Tuple [Term]
    | List [Term]
    | ListCons Term Term
    | Bin BinOp Term Term
    | Un UnaryOp Term
    | Seq [Term]
    | Error Term
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

      ppImports =
        if null imports then PP.empty
        else
          let ppLibName ((LibName s, _)) = text "import" <+> text s
          in
            (vcat $ (map ppLibName imports)) $$ PP.text ""
  in vcat [ ppImports
          , ppAtoms
          , ppTerm 0 term ]


ppTerm :: Precedence -> Term -> PP.Doc
ppTerm parentPrec t =
   let thisTermPrec = termPrec t
   in PP.maybeParens (thisTermPrec < parentPrec )
      $ ppTerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppTerm' :: Term -> PP.Doc
ppTerm' (Lit literal) = ppLit literal

ppTerm' (Error t) = text "error " PP.<> ppTerm' t

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

ppTerm' (Hnd hnd) =
  let (ppPat, ppSender, ppGuard, ppBody) = qqHandler hnd
  in  text "hn" <+> ppPat <+>
    (case ppSender of
         Just p -> text "|" <+> p
         Nothing -> PP.empty
    ) <+>
    (case ppGuard of
           Just p -> text "when" <+> p
           Nothing -> PP.empty)
      <+> text "=>"   <+> ppBody


ppTerm' (App t1 t2s) =
    ppTerm appPrec t1
          <+> (hsep (map (ppTerm argPrec) t2s))

ppTerm' (Let decs body) =
  text "let" <+>
  nest 3 (vcat (map ppDecl decs)) $$
  text "in" <+>
  nest 3 (ppTerm 0 body) $$
  text "end"


ppTerm' (Case e cases _) =
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
      ppDeclPattern decl <+> text "=>" <+> ppTerm 0 term



ppTerm' (If e0 e1 e2) =
  text "if" <+>
  ppTerm 0 e0 $$
  text "then" <+>
  ppTerm 0 e1 $$
  text "else" <+>
  ppTerm 0 e2

ppTerm' (Bin op t1 t2) =
  let binOpPrec = opPrec op
  in
     ppTerm binOpPrec t1 <+>
     text (show op) <+>
     ppTerm binOpPrec t2

ppTerm' (Un op t) =
  text (show op) <+> ppTerm' t

ppTerm' (Seq ts) = PP.hsep $
  PP.punctuate (text ";") (map ppTerm' ts)

qqLambda :: Lambda -> (PP.Doc, PP.Doc)
qqLambda (Lambda args body ) =
  let ppArgs' =
        if null args then text "()"
                     else hsep $ map ppDeclPattern args
  in ( ppArgs', ppTerm 0 body)


qqHandler :: Handler -> (PP.Doc, Maybe PP.Doc, Maybe PP.Doc, PP.Doc)
qqHandler (Handler pat Nothing Nothing e) =
  (ppDeclPattern pat, Nothing, Nothing, ppTerm 0 e)
qqHandler (Handler pat Nothing (Just g) e) =
  (ppDeclPattern pat, Nothing, (Just (ppTerm 0 g)), ppTerm 0 e)
qqHandler (Handler pat1 (Just pat2) Nothing e) =
  (ppDeclPattern pat1, Just (ppDeclPattern pat2), Nothing, ppTerm 0 e)
qqHandler (Handler pat1 (Just pat2) (Just g) e) =
  (ppDeclPattern pat1, Just (ppDeclPattern pat2), (Just (ppTerm 0 g)), ppTerm 0 e)


ppDecl :: Decl -> PP.Doc
ppDecl (ValDecl pattern t _) =
  text "val" <+> ppDeclPattern pattern <+> text "="
    <+> ppTerm 0 t
ppDecl (FunDecs fs) = ppFuns fs
  where
    ppFunDecl _ (FunDecl _ [] _) = error "empty fun list"
    ppFunDecl prefix (FunDecl fname (first:rest) _) =
      let ppFirstOption = ppFunOptions (prefix ++ " " ++ fname)
          ppOtherOption = ppFunOptions ("  | " ++ fname)
      in ppFirstOption first $$ vcat (map ppOtherOption rest)


    ppFunOptions prefix lam =
        let (ppArgs, ppBody) = qqLambda lam in
        text prefix <+> ppArgs <+> text "=" <+> nest 2 ppBody


    ppFuns (doc:docs) =
      let ppFirstFun = ppFunDecl "fun"
          ppOtherFun = ppFunDecl "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)


    ppFuns _ = PP.empty




ppDeclPattern :: DeclPattern -> PP.Doc
ppDeclPattern (VarPattern x) = text x
ppDeclPattern (Wildcard ) = text "_"
ppDeclPattern (AtPattern p l ) = ppDeclPattern p PP.<> text ("@ " ++ l)
ppDeclPattern (ValPattern literal ) = ppLit literal
ppDeclPattern (TuplePattern patterns ) =
  PP.parens $
  PP.hsep $
  PP.punctuate (text ",") (map ppDeclPattern patterns)
ppDeclPattern (ListPattern pats ) =
  PP.brackets $
  PP.hsep $
  PP.punctuate (text ",") (map ppDeclPattern pats)
ppDeclPattern (ConsPattern headPattern tailPattern ) =
  PP.parens $
  ppDeclPattern headPattern PP.<> text "::" PP.<> ppDeclPattern tailPattern


ppLit :: Lit -> PP.Doc
ppLit (LInt i _ )      = PP.integer i
ppLit (LString s )   = PP.doubleQuotes (text s)
ppLit (LUnit )       = text "()"
ppLit (LBool True  )  = text "true"
ppLit (LBool False) = text "false"
ppLit (LLabel s ) = PP.braces (text s)


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
opPrec FlowsTo    = 50
opPrec RaisedTo   = 50
opPrec And = 40
opPrec Or = 40


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
