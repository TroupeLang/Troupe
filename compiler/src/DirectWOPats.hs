module DirectWOPats ( Lambda (..)
              , Term (..)
              , LTerm
              , LVarName
              , Decl (..)
              , FunDecl (..)
              , Numeric(..)
              , Lit(..)
              , LFields
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
import DCLabels
import TroupePositionInfo (Located(..), unLoc, PosInf(..))
import PrettyPrint (PP, runPP, runPPDefault, ppLocated, ShowDebug(..))

-- | Located type aliases - all terms are wrapped in Located
type LTerm = Located Term
type LFields = [(FieldName, LTerm)]
-- | Located variable name - carries source position for variable bindings
type LVarName = Located VarName

data Decl
    = ValDecl VarName LTerm
    | FunDecs [FunDecl]
  deriving (Eq)

-- | Function declaration with name, lambda, and definition position
data FunDecl = FunDecl VarName Lambda PosInf
  deriving (Eq)

-- Numeric type represents integer and floating point numeric literals
data Numeric = NumInt Integer | NumFloat Double
  deriving (Eq, Ord, Show)

-- | Literals - note: position is NOT embedded in literals anymore,
-- it comes from the Located wrapper
data Lit
    = LNumeric Numeric
    | LString String
    | LLabel String
    | LDCLabel DCLabelExp
    | LUnit
    | LBool Bool
    | LAtom AtomName
  deriving (Eq, Show)

-- | Lambda - uses Located wrapper for body and for argument names
data Lambda = Lambda [LVarName] LTerm
  deriving (Eq)

-- | Term - no embedded positions, all position info is in Located wrapper
data Term
    = Lit Lit
    | Var VarName
    | Abs Lambda
    | App LTerm [LTerm]
    | Let [Decl] LTerm
    | If LTerm LTerm LTerm
    | AssertElseError LTerm LTerm LTerm    -- position from Located wrapper
    | Tuple [LTerm]
    | Record LFields
    | WithRecord LTerm LFields
    | ProjField LTerm FieldName
    | ProjIdx LTerm Word
    | List [LTerm]
    | ListCons LTerm LTerm
    | Bin BinOp LTerm LTerm
    | Un UnaryOp LTerm
    | Error LTerm                           -- position from Located wrapper
    deriving (Eq)

data Atoms = Atoms [AtomName]
      deriving (Eq, Show)

data Prog = Prog Imports Atoms LTerm
  deriving (Eq, Show)

-- Note: GetPosInfo for LTerm is provided by TroupePositionInfo's
-- instance GetPosInfo (Located a) which extracts position from Loc wrapper





--------------------------------------------------
-- show is defined via pretty printing
instance Show Term
  where show t = PP.render (runPPDefault (ppTerm 0 t))

instance ShowIndent Prog where
  showIndent k t = PP.render (nest k (runPPDefault (ppProg t)))

instance ShowDebug Prog where
  showDebugWith cfg = PP.render . runPP cfg . ppProg
--------------------------------------------------
-- obs: these functions are not exported
--



ppProg :: Prog -> PP PP.Doc
ppProg (Prog (Imports imports) (Atoms atoms) lterm) = do
  ltermDoc <- ppLTerm 0 lterm
  let ppAtoms =
        if null atoms
          then PP.empty
          else (text "datatype Atoms = ") <+>
               (hsep $ PP.punctuate (text " |") (map text atoms))
      ppImports = if null imports then PP.empty else text "<<imports>>\n"
  pure $ ppImports $$ ppAtoms $$ ltermDoc

-- | Pretty print a Located Term
ppLTerm :: Precedence -> LTerm -> PP PP.Doc
ppLTerm parentPrec = ppLocated (ppTerm parentPrec)

ppTerm :: Precedence -> Term -> PP PP.Doc
ppTerm parentPrec t = do
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

ppTerm' (Tuple lts) = do
  ds <- mapM (ppLTerm 0) lts
  pure $ PP.parens $ PP.hcat $ PP.punctuate (text ",") ds

ppTerm' (Record fs) = do
  fsDoc <- qqLFields fs
  pure $ PP.braces fsDoc

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


ppTerm' (List lts) = do
  ds <- mapM (ppLTerm 0) lts
  pure $ PP.brackets $ PP.hcat $ PP.punctuate (text ",") ds



ppTerm' (ListCons lhd ltl) = do
  hdDoc <- ppLTerm consPrec lhd
  tlDoc <- ppLTerm consPrec ltl
  pure $ hdDoc PP.<> text "::" PP.<> tlDoc

ppTerm' (Var x) = pure $ text x
ppTerm' (Abs lam) = do
  (ppArgs, ppBody) <- qqLambda lam
  pure $ text "fn" <+> ppArgs <+> text "=>" <+> ppBody

ppTerm' (App lt1 lt2s) = do
  d1 <- ppLTerm appPrec lt1
  d2s <- mapM (ppLTerm argPrec) lt2s
  pure $ d1 <+> (hsep d2s)

ppTerm' (Let decs lbody) = do
  decDocs <- mapM ppDecl decs
  bodyDoc <- ppLTerm 0 lbody
  pure $ text "let" <+>
    nest 3 (vcat decDocs) $$
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


-- | Pretty print LFields
qqLFields :: LFields -> PP PP.Doc
qqLFields fs = do
  fieldDocs <- mapM ppField fs
  pure $ PP.hcat $ PP.punctuate (text ",") fieldDocs
     where ppField (name, lt) = do
              d <- ppLTerm 0 lt
              pure $ PP.hcat [PP.text name, PP.text "=", d]


qqLambda :: Lambda -> PP (PP.Doc, PP.Doc)
qqLambda (Lambda args lbody) = do
  bodyDoc <- ppLTerm 0 lbody
  let ppArgs' =
        if null args then text "()"
                     else hsep $ map (text . unLoc) args
  pure (ppArgs', bodyDoc)

ppDecl :: Decl -> PP PP.Doc
ppDecl (ValDecl x lt) = do
  d <- ppLTerm 0 lt
  pure $ text "val" <+> text x <+> text "=" <+> d
ppDecl (FunDecs fs) = ppFuns =<< mapM ppFunDecl fs
  where
    ppFunDecl (FunDecl fname (Lambda args lbody) _) = do
      bodyDoc <- ppLTerm 0 lbody
      let ppArgs = if null args then text "()" else hsep (map (text . unLoc) args)
      pure (text fname <+> ppArgs <+> text "=", bodyDoc)
    ppFuns (doc:docs) =
      let pp' prefix (docHead, docBody) = text prefix <+> docHead $$ nest 2 docBody
          ppFirstFun = pp' "fun"
          ppOtherFun = pp' "and"
      in pure $ ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = pure PP.empty

ppLit :: Lit -> PP.Doc
ppLit (LNumeric (NumInt i))  = PP.integer i
ppLit (LNumeric (NumFloat f)) = PP.double f
ppLit (LString s)   = PP.doubleQuotes (text s)
ppLit (LLabel s)    = PP.braces (text s)
ppLit (LDCLabel dc) = ppDCLabelExpLit dc
ppLit LUnit         = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LAtom a) = text a




termPrec :: Term -> Precedence
termPrec (Lit _)           = maxPrec
termPrec (Tuple _)         = maxPrec
termPrec (List _)          = maxPrec
termPrec (Var _)           = maxPrec
termPrec (App _ _)         = appPrec
termPrec (Bin op _ _)      = opPrec op
termPrec (ListCons _ _)    = 200
termPrec _                 = 0
