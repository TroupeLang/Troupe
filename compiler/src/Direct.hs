module Direct ( Lambda (..)
              , Term (..)
              , Decl (..)
              , FunDecl (..)
              , Numeric(..)
              , Lit(..)
              , DeclPattern(..)
              , RecordPatternMode(..)
              , AtomName
              , Atoms(..)
              , Prog(..)
              , Handler(..)
              , FieldName
              , ppLit
              -- Located type aliases
              , LTerm
              , LDecl
              , LDeclPattern
              , LFunDecl
              , LFields
              )
where

import           Basics
import qualified Text.PrettyPrint.HughesPJ as PP
import DCLabels
import Text.PrettyPrint.HughesPJ (
    (<+>), ($$), text, hsep, vcat, nest)
import           ShowIndent
import           TroupePositionInfo (Located(..))


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



-- | Located type aliases for the Direct AST
-- These are defined before the data types to allow mutual recursion
type LTerm = Located Term
type LDecl = Located Decl
type LDeclPattern = Located DeclPattern
type LFunDecl = Located FunDecl

-- Numeric type represents integer and floating point numeric literals
data Numeric = NumInt Integer | NumFloat Double
  deriving (Eq, Ord, Show)

data Lit
    = LNumeric Numeric
    | LUnit
    | LBool Bool
    | LString String
    | LLabel String
    | LDCLabel DCLabelExp
    | LAtom AtomName
  deriving (Eq, Show)

data RecordPatternMode = ExactMatch | WildcardMatch
  deriving (Eq, Show)

data DeclPattern
    = VarPattern VarName
    | ValPattern Lit
    | AtPattern LDeclPattern String
    | Wildcard
    | TuplePattern [LDeclPattern]
    | ConsPattern LDeclPattern LDeclPattern
    | ListPattern [LDeclPattern]
    | RecordPattern [(FieldName, Maybe LDeclPattern)] RecordPatternMode
    | ErrorPattern                                    -- Error recovery placeholder
      deriving (Eq)

-- GetPosInfo for DeclPattern is no longer needed - use posInfo on LDeclPattern instead

data Lambda = Lambda [LDeclPattern] LTerm
  deriving (Eq)

type Guard = Maybe LTerm
data Handler = Handler LDeclPattern (Maybe LDeclPattern) Guard LTerm
  deriving (Eq)

data Decl
    = ValDecl LDeclPattern LTerm
    | FunDecs [LFunDecl]
    | ErrorDecl                   -- Error recovery placeholder
  deriving (Eq)

data FunDecl = FunDecl VarName [Lambda]
  deriving (Eq)

type LFields = [(FieldName, Maybe LTerm)]

data Term
    = Lit Lit
    | Var VarName
    | Abs Lambda
    | Hnd Handler
    | App LTerm [LTerm]
    | Let [Decl] LTerm
    | Case LTerm [(LDeclPattern, LTerm)]
    | If LTerm LTerm LTerm
    | Tuple [LTerm]
    | Record LFields
    | WithRecord LTerm LFields
    | ProjField LTerm FieldName
    | ProjIdx LTerm Word
    | List [LTerm]
    | ListCons LTerm LTerm
    | Bin BinOp LTerm LTerm
    | Un UnaryOp LTerm
    | Seq [LTerm]
    | Error LTerm
          deriving (Eq)

data Atoms = Atoms [AtomName]
      deriving (Eq, Show)


data Prog = Prog Imports Atoms LTerm
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
ppProg (Prog (Imports imports) (Atoms atoms) term) =
  let ppAtoms =
        if null atoms
          then PP.empty
          else (text "datatype Atoms = ") <+>
               (hsep $ PP.punctuate (text " |") (map text atoms))

      ppImports =
        if null imports then PP.empty
        else
          let ppLibName imp =
                let LibName s = importLib imp
                    modeText = case importMode imp of
                      Qualified -> text "import qualified" <+> text s
                      Unqualified -> text "import" <+> text s
                    selectText = case importSelected imp of
                      Just names -> text " only (" <> (hsep $ PP.punctuate (text ",") (map text names)) <> text ")"
                      Nothing -> PP.empty
                    aliasText = case importAlias imp of
                      Just (LibName a) -> text " as" <+> text a
                      Nothing -> PP.empty
                in modeText <> selectText <> aliasText
          in
            (vcat $ (map ppLibName imports)) $$ PP.text ""
  in vcat [ ppImports
          , ppAtoms
          , ppLTerm 0 term ]

-- | Pretty print a located term at given precedence
ppLTerm :: Precedence -> LTerm -> PP.Doc
ppLTerm prec (Loc _ t) = ppTerm prec t

ppTerm :: Precedence -> Term -> PP.Doc
ppTerm parentPrec t =
   let thisTermPrec = termPrec t
   in PP.maybeParens (thisTermPrec < parentPrec )
      $ ppTerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppTerm' :: Term -> PP.Doc
ppTerm' (Lit literal) = ppLit literal

ppTerm' (Error t) = text "error " PP.<> ppLTerm 0 t

ppTerm'  (Tuple ts) =
  PP.parens $
  PP.hcat $
  PP.punctuate (text ",") (map (ppLTerm 0) ts)

ppTerm' (Record fs) =
  PP.braces $ qqFields fs

ppTerm' (WithRecord t fs) =
  PP.braces $ PP.hsep [ppLTerm 0 t, text "with", qqFields fs]


ppTerm' (ProjField t fn) =
  ppLTerm projPrec t PP.<> text "." PP.<> PP.text fn

ppTerm' (ProjIdx t idx) =
  ppLTerm projPrec t PP.<> text "." PP.<> PP.text (show idx)

ppTerm'  (List ts) =
  PP.brackets $
  PP.hcat $
  PP.punctuate (text ",") (map (ppLTerm 0) ts)

ppTerm' (ListCons hd tl) =
   ppLTerm consPrec hd PP.<> text "::" PP.<> ppLTerm consPrec tl

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
    ppLTerm appPrec t1
          <+> (hsep (map (ppLTerm argPrec) t2s))

ppTerm' (Let decs body) =
  text "let" <+>
  nest 3 (vcat (map ppDecl decs)) $$
  text "in" <+>
  nest 3 (ppLTerm 0 body) $$
  text "end"


ppTerm' (Case e cases) =
  text "case" <+>
  ppLTerm 0 e  $$
  nest 2 (ppCases cases)
  where
    ppCases [] = error "empty cases"
    ppCases (first:rest) =
      text "of" <+> ppCaseBody first $$
      vcat (map ppNonFirst rest)

    ppNonFirst second =
      text " |" <+> ppCaseBody second

    ppCaseBody (decl, term) =
      ppLDeclPattern decl <+> text "=>" <+> ppLTerm 0 term



ppTerm' (If e0 e1 e2) =
  text "if" <+>
  ppLTerm 0 e0 $$
  text "then" <+>
  ppLTerm 0 e1 $$
  text "else" <+>
  ppLTerm 0 e2

ppTerm' (Bin op t1 t2) =
  let binOpPrec = opPrec op
  in
     ppLTerm binOpPrec t1 <+>
     text (show op) <+>
     ppLTerm binOpPrec t2

ppTerm' (Un op t) =
  text (show op) <+> ppLTerm 0 t

ppTerm' (Seq ts) = PP.hsep $
  PP.punctuate (text ";") (map (ppLTerm 0) ts)

qqLambda :: Lambda -> (PP.Doc, PP.Doc)
qqLambda (Lambda args body) =
  let ppArgs' =
        if null args then text "()"
                     else hsep $ map ppLDeclPattern args
  in ( ppArgs', ppLTerm 0 body)


qqFields :: LFields -> PP.Doc
qqFields fs = PP.hcat $
    PP.punctuate (text ",") (map ppField fs)
     where ppField (name, Nothing) = PP.text name
           ppField (name, Just t)  =
              PP.hcat [PP.text name, PP.text "=", ppLTerm 0 t ]


qqHandler :: Handler -> (PP.Doc, Maybe PP.Doc, Maybe PP.Doc, PP.Doc)
qqHandler (Handler pat Nothing Nothing e) =
  (ppLDeclPattern pat, Nothing, Nothing, ppLTerm 0 e)
qqHandler (Handler pat Nothing (Just g) e) =
  (ppLDeclPattern pat, Nothing, (Just (ppLTerm 0 g)), ppLTerm 0 e)
qqHandler (Handler pat1 (Just pat2) Nothing e) =
  (ppLDeclPattern pat1, Just (ppLDeclPattern pat2), Nothing, ppLTerm 0 e)
qqHandler (Handler pat1 (Just pat2) (Just g) e) =
  (ppLDeclPattern pat1, Just (ppLDeclPattern pat2), (Just (ppLTerm 0 g)), ppLTerm 0 e)


ppDecl :: Decl -> PP.Doc
ppDecl (ValDecl pattern t) =
  text "val" <+> ppLDeclPattern pattern <+> text "="
    <+> ppLTerm 0 t
ppDecl (FunDecs fs) = ppFuns fs
  where
    ppLFunDecl _ (Loc _ (FunDecl _ [])) = error "empty fun list"
    ppLFunDecl prefix (Loc _ (FunDecl fname (first:rest))) =
      let ppFirstOption = ppFunOptions (prefix ++ " " ++ fname)
          ppOtherOption = ppFunOptions ("  | " ++ fname)
      in ppFirstOption first $$ vcat (map ppOtherOption rest)


    ppFunOptions prefix lam =
        let (ppArgs, ppBody) = qqLambda lam in
        text prefix <+> ppArgs <+> text "=" <+> nest 2 ppBody


    ppFuns (doc:docs) =
      let ppFirstFun = ppLFunDecl "fun"
          ppOtherFun = ppLFunDecl "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)


    ppFuns _ = PP.empty

ppDecl ErrorDecl = text "<error-decl>"

-- | Pretty print a located declaration pattern
ppLDeclPattern :: LDeclPattern -> PP.Doc
ppLDeclPattern (Loc _ p) = ppDeclPattern p

ppDeclPattern :: DeclPattern -> PP.Doc
ppDeclPattern (VarPattern x) = text x
ppDeclPattern Wildcard = text "_"
ppDeclPattern (AtPattern p l) = ppLDeclPattern p PP.<> text ("@ " ++ l)
ppDeclPattern (ValPattern literal) = ppLit literal
ppDeclPattern (TuplePattern patterns) =
  PP.parens $
  PP.hsep $
  PP.punctuate (text ",") (map ppLDeclPattern patterns)
ppDeclPattern (ListPattern pats) =
  PP.brackets $
  PP.hsep $
  PP.punctuate (text ",") (map ppLDeclPattern pats)
ppDeclPattern (ConsPattern headPattern tailPattern) =
  PP.parens $
  ppLDeclPattern headPattern PP.<> text "::" PP.<> ppLDeclPattern tailPattern
ppDeclPattern (RecordPattern fields mode) =
  PP.braces $
    PP.hsep $
      PP.punctuate (text ",") (map ppField fields ++ wildcard)
        where ppField (f, Nothing) = text f
              ppField (f, Just pat) = PP.hsep[text f, text "=", ppLDeclPattern pat]
              wildcard = case mode of
                ExactMatch -> []
                WildcardMatch -> [text ".."]
ppDeclPattern ErrorPattern = text "<error>"

ppLit :: Lit -> PP.Doc
ppLit (LNumeric (NumInt i))  = PP.integer i
ppLit (LNumeric (NumFloat f)) = PP.double f
ppLit (LString s)   = PP.doubleQuotes (text s)
ppLit (LDCLabel dc) = ppDCLabelExp dc
ppLit LUnit       = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LLabel s) = PP.braces (text s)
ppLit (LAtom s) = text s


termPrec :: Term -> Precedence
termPrec (Lit _)         = maxPrec
termPrec (Tuple _)       = maxPrec
termPrec (List _)        = maxPrec
termPrec (Var _)         = maxPrec
termPrec (App _ _)       = appPrec
termPrec (Bin op _ _)    = opPrec op
termPrec (ProjField _ _) = projPrec
termPrec (ProjIdx _ _)   = projPrec
termPrec (ListCons _ _)  = 200
termPrec _               = 0
