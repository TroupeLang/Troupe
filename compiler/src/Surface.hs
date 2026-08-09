-- | The parse-phase AST. The parser targets this module and nothing else
-- does: expressions here have flat operator chains ('OpChain'), a dedicated
-- unary-minus form ('Neg'), and no 'Direct.Bin' / 'Direct.Un' /
-- 'Direct.ListCons' nodes at all — the parser cannot produce them, and the
-- type says so. The re-association pass (OpReassoc) translates 'Prog' into
-- 'Direct.Prog' once operator fixities are known; everything from
-- syntactic-variant folding onward consumes 'Direct' unchanged.
--
-- Patterns get the same treatment as terms: 'Pattern' here carries flat
-- 'PatChain's, 'Direct.DeclPattern' has no such constructor, and the two are
-- bridged only by re-association. An unresolved pattern chain therefore cannot
-- reach syntactic-variant folding or pattern-match elimination -- not by
-- convention, but because there is no constructor for it in the type those
-- passes consume.
--
-- The literal and @datatype@-surface layers contain no terms and are shared
-- with 'Direct' by re-export.
--
-- Design: @_dev_planning/_archive/custom-operators/design.md@ §4.2.
module Surface ( Lambda (..)
               , Term (..)
               , Decl (..)
               , FunDecl (..)
               , Prog (..)
               , FixityDecl (..)
               , Handler (..)
               , Guard
               -- Operator chains
               , ChainElem (..)
               , ChainOp (..)
               -- Patterns (parse phase: chains unresolved)
               , Pattern (..)
               , PatChainElem (..)
               , LPattern
               -- Located type aliases
               , LTerm
               , LDecl
               , LFunDecl
               , LFields
               -- Re-exported term-free layers (shared with Direct)
               , D.Numeric (..)
               , D.Lit (..)
               , D.v1LabelLit
               , D.RecordPatternMode (..)
               , D.FieldName
               , D.QName
               , D.SynTyExp (..)
               , D.SynCtor (..)
               , D.SynDataDecl (..)
               , D.SynDataGroup (..)
               )
where

import           Basics
import qualified Direct as D
import           Direct ( Lit, QName, SynDataGroup )
import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.PrettyPrint.HughesPJ ((<+>), ($$), text, hsep, vcat, nest)
import           ShowIndent
import           TroupePositionInfo (Located(..), PosInf(..))

type LTerm    = Located Term
type LDecl    = Located Decl
type LFunDecl = Located FunDecl
type LFields  = [(FieldName, Maybe LTerm)]

data Lambda = Lambda [LPattern] LTerm
  deriving (Eq)

type Guard = Maybe LTerm
data Handler = Handler LPattern (Maybe LPattern) Guard LTerm
  deriving (Eq)

data Decl
    = ValDecl LPattern LTerm
    | FunDecs [LFunDecl]
    | ErrorDecl
  deriving (Eq)

data FunDecl = FunDecl VarName [Lambda]
  deriving (Eq)

-- | One element of a flat operator chain, in source order. The parser records
-- operands, infix operators, and the prefix keyword operators
-- (@isTuple@/@isList@/@isRecord@/@not@) without committing to any grouping;
-- grouping is the re-association pass's job.
data ChainElem
    = ChOperand LTerm
    | ChInfix PosInf ChainOp
    | ChPrefix PosInf UnaryOp
  deriving (Eq)

-- | An infix operator occurrence: a built-in binary operator, the list
-- constructor, or a user-defined operator by name.
data ChainOp
    = ChBin BinOp
    | ChCons
    | ChUser VarName
  deriving (Eq)

data Term
    = Lit Lit
    | Var VarName
    | Abs Lambda
    | Hnd Handler
    | App LTerm [LTerm]
    | Let [Decl] LTerm
    | Case LTerm [(LPattern, LTerm)]
    | If LTerm LTerm LTerm
    | Tuple [LTerm] SynVariantTag
    | Record LFields
    | WithRecord LTerm LFields
    | ProjField LTerm FieldName
    | ProjIdx LTerm Word
    | List [LTerm]
    | Seq [LTerm]
    | Neg LTerm                -- unary minus (operand-level, maximally tight)
    | OpChain [ChainElem]      -- at least one operator or prefix item
  deriving (Eq)

-- | One fixity declaration from the file header: @infixl 6 <+> <.>@ declares
-- every listed operator at the given fixity. The position is the keyword's,
-- for diagnostics.
-- | A pattern as parsed. Every constructor mirrors 'Direct.DeclPattern' except
-- the two that exist only before re-association:
--
--   * 'PatChain' -- the infix layer left flat, resolved against the same fixity
--     table expressions use, so patterns and expressions cannot disagree about
--     an operator's precedence.
--   * 'ErrorPattern' -- the parse-recovery placeholder.
--
-- Juxtaposition (constructor application) is resolved by the grammar, exactly
-- as application is for terms, so it arrives here already as 'ConPattern'.
data Pattern
    = VarPattern VarName
    | ValPattern Lit
    | AtPattern LPattern String
    | Wildcard
    | TuplePattern [LPattern]
    | ListPattern [LPattern]
    | RecordPattern [(FieldName, Maybe LPattern)] D.RecordPatternMode
    | ConPattern QName (Maybe LPattern)
    | PatChain [PatChainElem]        -- at least one infix occurrence
    | ErrorPattern                   -- parse-error recovery placeholder
  deriving (Eq)

-- | One element of a flat pattern chain, in source order -- the pattern
-- counterpart of 'ChainElem'. Patterns have no prefix operators, so there is
-- no analogue of 'ChPrefix'.
data PatChainElem
    = PatOperand LPattern
    | PatInfix PosInf ChainOp
  deriving (Eq)

type LPattern = Located Pattern

data FixityDecl = FixityDecl PosInf Fixity [VarName]
  deriving (Eq)

data Prog = Prog Imports [FixityDecl] [SynDataGroup] LTerm
  deriving (Eq)

--------------------------------------------------
-- Printing: the out.syntax dump. Chains print flat, exactly as written.

-- | Print a parse-phase pattern. Everything but the chain prints as 'Direct'
-- prints it; a chain prints flat, in source order, like a term chain.
ppLPattern :: LPattern -> PP.Doc
ppLPattern (Loc _ p) = ppPattern p

ppPattern :: Pattern -> PP.Doc
ppPattern (VarPattern x)      = D.ppName x
ppPattern Wildcard            = text "_"
ppPattern (AtPattern p l)     = ppLPattern p PP.<> text ("@ " ++ l)
ppPattern (ValPattern lit)    = D.ppLit lit
ppPattern (TuplePattern ps)   = PP.parens (PP.hsep (PP.punctuate (text ",") (map ppLPattern ps)))
ppPattern (ListPattern ps)    = PP.brackets (PP.hsep (PP.punctuate (text ",") (map ppLPattern ps)))
ppPattern (ConPattern qname mpayload) =
  let con = text (foldr1 (\a b -> a ++ "." ++ b) qname)
  in case mpayload of
       Nothing -> con
       Just p  -> PP.parens (con <+> ppLPattern p)
ppPattern (RecordPattern fields mode) =
  PP.braces (PP.hsep (PP.punctuate (text ",") (map ppField fields ++ wildcard)))
  where ppField (f, Nothing)  = text f
        ppField (f, Just pat) = PP.hsep [text f, text "=", ppLPattern pat]
        wildcard = case mode of
                     D.ExactMatch    -> []
                     D.WildcardMatch -> [text ".."]
ppPattern (PatChain elems)    = PP.hsep (map ppPatElem elems)
  where ppPatElem (PatOperand p)  = ppLPattern p
        ppPatElem (PatInfix _ op) = text (ppChainOp op)
ppPattern ErrorPattern        = text "<error>"

instance Show Term where
  show t = PP.render (ppTerm 0 t)

instance ShowIndent Prog where
  showIndent k t = PP.render (nest k (ppProg t))

ppProg :: Prog -> PP.Doc
ppProg (Prog (Imports imports) fixities groups term) =
  let ppFixities =
        if null fixities then PP.empty
        else vcat (map ppFixityDecl fixities)
      ppFixityDecl (FixityDecl _ (Fixity a n) ops) =
        text (case a of OpLeft -> "infixl"; OpRight -> "infixr"; OpNon -> "infix")
          <+> PP.int n <+> hsep (map text ops)
      ppGroups =
        if null groups then PP.empty
        else vcat (map D.ppSynDataGroup groups)
      ppImports =
        if null imports then PP.empty
        else
          let ppLibName imp =
                let LibName ln = importLib imp
                    s = case importPath imp of
                          Just p  -> "\"" ++ p ++ "\""
                          Nothing -> ln
                    modeText = case importMode imp of
                      Qualified -> text "import qualified" <+> text s
                      Unqualified -> text "import" <+> text s
                    selectText = case importSelected imp of
                      Just names -> text " only (" PP.<> (hsep $ PP.punctuate (text ",") (map text names)) PP.<> text ")"
                      Nothing -> PP.empty
                    aliasText = case importAlias imp of
                      Just (LibName a) -> text " as" <+> text a
                      Nothing -> PP.empty
                in modeText PP.<> selectText PP.<> aliasText
          in (vcat $ (map ppLibName imports)) $$ PP.text ""
  in vcat [ ppImports, ppFixities, ppGroups, ppLTerm 0 term ]

ppLTerm :: Precedence -> LTerm -> PP.Doc
ppLTerm prec (Loc _ t) = ppTerm prec t

ppTerm :: Precedence -> Term -> PP.Doc
ppTerm parentPrec t =
  let thisTermPrec = termPrec t
  in PP.maybeParens (thisTermPrec < parentPrec) $ ppTerm' t

ppTerm' :: Term -> PP.Doc
ppTerm' (Lit literal) = D.ppLit literal
ppTerm' (Tuple ts _) =
  PP.parens $ PP.hcat $ PP.punctuate (text ",") (map (ppLTerm 0) ts)
ppTerm' (Record fs) = PP.braces $ qqFields fs
ppTerm' (WithRecord t fs) =
  PP.braces $ PP.hsep [ppLTerm 0 t, text "with", qqFields fs]
ppTerm' (ProjField t fn) =
  ppLTerm projPrec t PP.<> text "." PP.<> PP.text fn
ppTerm' (ProjIdx t idx) =
  ppLTerm projPrec t PP.<> text "." PP.<> PP.text (show idx)
ppTerm' (List ts) =
  PP.brackets $ PP.hcat $ PP.punctuate (text ",") (map (ppLTerm 0) ts)
ppTerm' (Var x) = D.ppName x
ppTerm' (Abs lam) =
  let (ppArgs, ppBody) = qqLambda lam
  in text "fn" <+> ppArgs <+> text "=>" <+> ppBody
ppTerm' (Hnd hnd) =
  let (ppPat, ppSender, ppGuard, ppBody) = qqHandler hnd
  in text "hn" <+> ppPat <+>
     (case ppSender of
        Just p -> text "|" <+> p
        Nothing -> PP.empty) <+>
     (case ppGuard of
        Just p -> text "when" <+> p
        Nothing -> PP.empty)
     <+> text "=>" <+> ppBody
ppTerm' (App t1 t2s) =
  ppLTerm appPrec t1 <+> (hsep (map (ppLTerm argPrec) t2s))
ppTerm' (Let decs body) =
  text "let" <+>
  nest 3 (vcat (map ppDecl decs)) $$
  text "in" <+>
  nest 3 (ppLTerm 0 body) $$
  text "end"
ppTerm' (Case e cases) =
  text "case" <+> ppLTerm 0 e $$ nest 2 (ppCases cases)
  where
    ppCases [] = error "empty cases"
    ppCases (first:rest) =
      text "of" <+> ppCaseBody first $$ vcat (map ppNonFirst rest)
    ppNonFirst second = text " |" <+> ppCaseBody second
    ppCaseBody (decl, term) =
      ppLPattern decl <+> text "=>" <+> ppLTerm 0 term
ppTerm' (If e0 e1 e2) =
  text "if" <+> ppLTerm 0 e0 $$
  text "then" <+> ppLTerm 0 e1 $$
  text "else" <+> ppLTerm 0 e2
ppTerm' (Seq ts) =
  PP.hsep $ PP.punctuate (text ";") (map (ppLTerm 0) ts)
ppTerm' (Neg t) = text "-" PP.<> ppLTerm maxPrec t
ppTerm' (OpChain elems) = hsep (map ppElem elems)
  where
    ppElem (ChOperand t)  = ppLTerm chainOperandPrec t
    ppElem (ChInfix _ op) = text (ppChainOp op)
    ppElem (ChPrefix _ u) = text (show u)
    chainOperandPrec = appPrec

ppChainOp :: ChainOp -> String
ppChainOp (ChBin op) = show op
ppChainOp ChCons     = "::"
ppChainOp (ChUser v) = v

qqLambda :: Lambda -> (PP.Doc, PP.Doc)
qqLambda (Lambda args body) =
  let ppArgs' =
        if null args then text "()"
        else hsep $ map ppLPattern args
  in (ppArgs', ppLTerm 0 body)

qqFields :: LFields -> PP.Doc
qqFields fs = PP.hcat $ PP.punctuate (text ",") (map ppField fs)
  where ppField (name, Nothing) = PP.text name
        ppField (name, Just t)  =
          PP.hcat [PP.text name, PP.text "=", ppLTerm 0 t]

qqHandler :: Handler -> (PP.Doc, Maybe PP.Doc, Maybe PP.Doc, PP.Doc)
qqHandler (Handler pat mpat g e) =
  ( ppLPattern pat
  , fmap ppLPattern mpat
  , fmap (ppLTerm 0) g
  , ppLTerm 0 e )

ppDecl :: Decl -> PP.Doc
ppDecl (ValDecl pattern t) =
  text "val" <+> ppLPattern pattern <+> text "=" <+> ppLTerm 0 t
ppDecl (FunDecs fs) = ppFuns fs
  where
    ppLFunDecl _ (Loc _ (FunDecl _ [])) = error "empty fun list"
    ppLFunDecl prefix (Loc _ (FunDecl fname (first:rest))) =
      let ppFirstOption = ppFunOptions (prefix ++ " " ++ D.nameStr fname)
          ppOtherOption = ppFunOptions ("  | " ++ D.nameStr fname)
      in ppFirstOption first $$ vcat (map ppOtherOption rest)
    ppFunOptions prefix lam =
      let (ppArgs, ppBody) = qqLambda lam
      in text prefix <+> ppArgs <+> text "=" <+> nest 2 ppBody
    ppFuns (doc:docs) =
      let ppFirstFun = ppLFunDecl "fun"
          ppOtherFun = ppLFunDecl "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)
    ppFuns _ = PP.empty
ppDecl ErrorDecl = text "<error-decl>"

termPrec :: Term -> Precedence
termPrec (Lit _)         = maxPrec
termPrec (Tuple _ _)     = maxPrec
termPrec (List _)        = maxPrec
termPrec (Var _)         = maxPrec
termPrec (App _ _)       = appPrec
termPrec (ProjField _ _) = projPrec
termPrec (ProjIdx _ _)   = projPrec
termPrec (Neg _)         = appPrec
termPrec _               = 0
