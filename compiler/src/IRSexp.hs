-- | Textual s-expression format for the Troupe IR (troupe-ir-sexp).
--
-- This module implements the format specified in
-- @compiler/docs/spec-troupe-ir-sexp.md@:
--
--   * 'printProg' : 'IRProgram' -> 'String' — the canonical printer (R2).
--   * 'parseProg' : 'String' -> 'Either' 'String' 'IRProgram' — the parser
--     (tolerant lexically, strict on the versioned @(troupe-ir-sexp 1 ...)@
--     wrapper).
--   * 'erasePosProg' — normalizes all source positions to 'NoPos' so the
--     round-trip law R1 can be stated over position-erased ASTs.
--
-- The implementation has two layers: a small s-expression datum reader
-- ('Datum', 'tokenize', 'parseTop') and an IR decoder/encoder over datums.
module IRSexp
  ( printProg
  , parseProg
  , erasePosProg
  , Datum(..)
  ) where

import           Data.Char (isSpace, isHexDigit, digitToInt)
import           Numeric (showHex)
import           Text.Read (readMaybe)
import qualified Text.PrettyPrint.HughesPJ as PP

import           IR
import qualified Core as C
import           Basics (BinOp(..), UnaryOp(..), LibName(..), FieldName)
import           DCLabels
import           RetCPS (VarName(..))
import           TroupePositionInfo (Located(..), PosInf(..), noLoc, unLoc)

------------------------------------------------------------
-- The current format version.
------------------------------------------------------------

formatVersion :: Integer
formatVersion = 1

------------------------------------------------------------
-- S-expression datum: the lexical/structural layer.
------------------------------------------------------------

-- | An s-expression datum. 'Atom' is a bare token (a symbol, number,
-- @#true@/@#false@, @self@, @unit@, ...); 'Str' is the (unescaped) content of a
-- double-quoted string; 'Lst' is a parenthesized list.
data Datum
  = Atom String
  | Str String
  | Lst [Datum]
  deriving (Eq, Show)

------------------------------------------------------------
-- Operator name tables (Haskell constructor names, verbatim).
------------------------------------------------------------

binOpTable :: [(String, BinOp)]
binOpTable =
  [ ("Plus", Plus), ("Minus", Minus), ("Mult", Mult), ("Div", Div)
  , ("Mod", Mod), ("Eq", Eq), ("Neq", Neq), ("Le", Le), ("Lt", Lt)
  , ("Ge", Ge), ("Gt", Gt), ("And", And), ("Or", Or)
  , ("RaisedTo", RaisedTo), ("Concat", Concat)
  , ("IntDiv", IntDiv), ("BinAnd", BinAnd), ("BinOr", BinOr)
  , ("BinXor", BinXor), ("BinShiftLeft", BinShiftLeft)
  , ("BinShiftRight", BinShiftRight), ("BinZeroShiftRight", BinZeroShiftRight)
  , ("HasField", HasField), ("LatticeJoin", LatticeJoin)
  ]

unOpTable :: [(String, UnaryOp)]
unOpTable =
  [ ("IsList", IsList), ("IsTuple", IsTuple), ("IsRecord", IsRecord)
  , ("Head", Head), ("Tail", Tail)
  , ("ListLength", ListLength), ("TupleLength", TupleLength)
  , ("RecordSize", RecordSize), ("LevelOf", LevelOf)
  , ("UnMinus", UnMinus), ("Not", Not)
  ]

nameOf :: Eq a => String -> [(String, a)] -> a -> String
nameOf what tbl x =
  case [ n | (n, y) <- tbl, y == x ] of
    (n : _) -> n
    []      -> error ("IRSexp.nameOf: missing " ++ what)

binOpName :: BinOp -> String
binOpName = nameOf "BinOp" binOpTable

unOpName :: UnaryOp -> String
unOpName = nameOf "UnaryOp" unOpTable

-- | The maximum ProjIdx value permitted by IR.hs (2^31 - 1).
maxProjIdx :: Integer
maxProjIdx = 2147483647

------------------------------------------------------------
-- Encoder: IRProgram -> Datum
------------------------------------------------------------

encodeProg :: IRProgram -> Datum
encodeProg p =
  Lst [ Atom "troupe-ir-sexp"
      , Atom (show formatVersion)
      , encodeProgBody p
      ]

encodeProgBody :: IRProgram -> Datum
encodeProgBody (IRProgram atoms funs) =
  Lst (Atom "program" : encodeAtoms atoms : map (encodeFun . unLoc) funs)

encodeAtoms :: C.Atoms -> Datum
encodeAtoms (C.Atoms names) = Lst (Atom "atoms" : map Str names)

encodeFun :: FunDef -> Datum
encodeFun (FunDef (HFN name) (Loc _ (VN arg)) consts body) =
  Lst [ Atom "fun"
      , Str name
      , Lst [Atom "arg", Str arg]
      , encodeConsts consts
      , encodeBB body
      ]

encodeConsts :: Consts -> Datum
encodeConsts consts =
  Lst (Atom "consts" : map enc consts)
  where enc (VN v, lit) = Lst [Str v, encodeLit lit]

encodeBB :: IRBBTree -> Datum
encodeBB (BB insts term) =
  Lst [ Atom "bb"
      , Lst (map (encodeInst . unLoc) insts)
      , encodeTerm (unLoc term)
      ]

encodeInst :: IRInst -> Datum
encodeInst (Assign (VN v) e) =
  Lst [Atom "assign", Str v, encodeExpr e]
encodeInst (MkFunClosures caps clos) =
  Lst [ Atom "mkclos"
      , Lst (map encCap caps)
      , Lst (map encClo clos)
      ]
  where encCap (VN v, lva) = Lst [Str v, encodeLVA lva]
        encClo (VN v, HFN h) = Lst [Str v, Str h]

encodeExpr :: IRExpr -> Datum
encodeExpr (Bin op a b) =
  Lst [Atom "bin", Atom (binOpName op), encodeLVA a, encodeLVA b]
encodeExpr (Un op a) =
  Lst [Atom "un", Atom (unOpName op), encodeLVA a]
encodeExpr (Tuple vas) =
  Lst (Atom "tuple" : map encodeLVA vas)
encodeExpr (Record fields) =
  Lst (Atom "record" : map encField fields)
encodeExpr (WithRecord lva fields) =
  Lst (Atom "with-record" : encodeLVA lva : map encField fields)
encodeExpr (ProjField lva f) =
  Lst [Atom "proj-field", encodeLVA lva, Str f]
encodeExpr (ProjIdx lva w) =
  Lst [Atom "proj-idx", encodeLVA lva, Atom (show w)]
encodeExpr (List vas) =
  Lst (Atom "list" : map encodeLVA vas)
encodeExpr (ListCons a b) =
  Lst [Atom "cons", encodeLVA a, encodeLVA b]
encodeExpr (Const lit) =
  Lst [Atom "const", encodeLit lit]
encodeExpr (Base v) =
  Lst [Atom "base", Str v]
encodeExpr (Lib (LibName l) v) =
  Lst [Atom "lib", Str l, Str v]

encField :: (FieldName, LVarAccess) -> Datum
encField (name, lva) = Lst [Str name, encodeLVA lva]

encodeTerm :: IRTerminator -> Datum
encodeTerm (TailCall f a) =
  Lst [Atom "tail-call", encodeLVA f, encodeLVA a]
encodeTerm (Ret a) =
  Lst [Atom "ret", encodeLVA a]
encodeTerm (If c t e) =
  Lst [Atom "if", encodeLVA c, encodeBB t, encodeBB e]
encodeTerm (AssertElseError c bb err) =
  Lst [Atom "assert-else-error", encodeLVA c, encodeBB bb, encodeLVA err]
encodeTerm (LibExport a) =
  Lst [Atom "lib-export", encodeLVA a]
encodeTerm (Error a) =
  Lst [Atom "error", encodeLVA a]
encodeTerm (StackExpand (VN v) b1 b2) =
  Lst [Atom "stack-expand", Str v, encodeBB b1, encodeBB b2]

encodeLVA :: LVarAccess -> Datum
encodeLVA = encodeVA . unLoc

encodeVA :: VarAccess -> Datum
encodeVA (VarLocal (VN v)) = Lst [Atom "local", Str v]
encodeVA (VarEnv (VN v))   = Lst [Atom "env", Str v]
encodeVA VarFunSelfRef     = Atom "self"

encodeLit :: C.Lit -> Datum
encodeLit (C.LNumeric (C.NumInt i))   = Lst [Atom "int", Atom (show i)]
encodeLit (C.LNumeric (C.NumFloat d)) = Lst [Atom "float", Atom (show d)]
encodeLit (C.LString s)               = Lst [Atom "string", Str s]
encodeLit (C.LBool b)                 = Lst [Atom "bool", Atom (if b then "true" else "false")]
encodeLit C.LUnit                     = Atom "unit"
encodeLit (C.LAtom a)                 = Lst [Atom "atom", Str a]
encodeLit (C.LLabel s)                = Lst [Atom "label-string", Str s]
encodeLit (C.LDCLabel dc)             = encodeDCLabel dc

encodeDCLabel :: DCLabelExp -> Datum
encodeDCLabel (DCLabelExp (c, i)) =
  Lst [Atom "dclabel", encodeComponent c, encodeComponent i]

encodeComponent :: LabelComponent -> Datum
encodeComponent (ConstComponent LabelTrue)  = Atom "#true"
encodeComponent (ConstComponent LabelFalse) = Atom "#false"
encodeComponent (ExprComponent le)          = encodeLabelExp le

encodeLabelExp :: LabelExp -> Datum
encodeLabelExp (TagExp t)        = Lst [Atom "tag", Str t]
encodeLabelExp (OpExp Conj a b)  = Lst [Atom "and", encodeLabelExp a, encodeLabelExp b]
encodeLabelExp (OpExp Disj a b)  = Lst [Atom "or", encodeLabelExp a, encodeLabelExp b]

------------------------------------------------------------
-- Rendering a Datum to canonical text.
------------------------------------------------------------

-- | Print a whole program in the canonical wrapped s-expression form.
printProg :: IRProgram -> String
printProg p = PP.renderStyle sty (ppDatum (encodeProg p)) ++ "\n"
  where sty = PP.style { PP.lineLength = 100 }

ppDatum :: Datum -> PP.Doc
ppDatum (Atom s) = PP.text s
ppDatum (Str s)  = PP.text (showStr s)
ppDatum (Lst []) = PP.text "()"
ppDatum (Lst ds) = PP.parens (PP.sep (map ppDatum ds))

showStr :: String -> String
showStr s = '"' : concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c
      | c < ' '   = "\\u" ++ pad4 (showHex (fromEnum c) "")
      | otherwise = [c]
    pad4 h = replicate (4 - length h) '0' ++ h

------------------------------------------------------------
-- Tokenizer + datum reader: the lexical/structural layer.
------------------------------------------------------------

data Token = TOpen | TClose | TAtom String | TStr String
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize = go
  where
    go [] = Right []
    go (c : cs)
      | isSpace c = go cs
      | c == ';'  = go (dropWhile (/= '\n') cs)
      | c == '('  = (TOpen :) <$> go cs
      | c == ')'  = (TClose :) <$> go cs
      | c == '"'  = do (s, rest) <- lexString cs
                       (TStr s :) <$> go rest
      | otherwise = let (tok, rest) = span isAtomChar (c : cs)
                    in (TAtom tok :) <$> go rest
    isAtomChar ch = not (isSpace ch) && ch `notElem` "()\";"

lexString :: String -> Either String (String, String)
lexString = go []
  where
    go _   []            = Left "unterminated string literal"
    go acc ('"' : rest)  = Right (reverse acc, rest)
    go acc ('\\' : c : rest) =
      case c of
        '"'  -> go ('"'  : acc) rest
        '\\' -> go ('\\' : acc) rest
        'n'  -> go ('\n' : acc) rest
        't'  -> go ('\t' : acc) rest
        'r'  -> go ('\r' : acc) rest
        'u'  -> case rest of
                  (a : b : d : e : rest')
                    | all isHexDigit [a, b, d, e] ->
                        go (toEnum (hex4 [a, b, d, e]) : acc) rest'
                  _ -> Left "bad \\u escape in string literal"
        _    -> Left ("bad escape sequence: \\" ++ [c])
    go _   ['\\']        = Left "dangling backslash in string literal"
    go acc (c : rest)    = go (c : acc) rest

    hex4 = foldl (\a c -> a * 16 + digitToInt c) 0

parseTop :: [Token] -> Either String Datum
parseTop ts = do
  (d, rest) <- parseOne ts
  case rest of
    [] -> Right d
    _  -> Left "trailing tokens after top-level datum"

parseOne :: [Token] -> Either String (Datum, [Token])
parseOne []              = Left "unexpected end of input"
parseOne (TStr s : ts)   = Right (Str s, ts)
parseOne (TAtom s : ts)  = Right (Atom s, ts)
parseOne (TClose : _)    = Left "unexpected )"
parseOne (TOpen : ts)    = do (ds, ts') <- parseList ts
                              Right (Lst ds, ts')

parseList :: [Token] -> Either String ([Datum], [Token])
parseList []             = Left "unterminated list (missing ')')"
parseList (TClose : ts)  = Right ([], ts)
parseList ts             = do (d, ts')   <- parseOne ts
                              (ds, ts'') <- parseList ts'
                              Right (d : ds, ts'')

------------------------------------------------------------
-- Decoder: Datum -> IRProgram
------------------------------------------------------------

-- | Parse a whole program from its canonical (or tolerant) s-expression text.
parseProg :: String -> Either String IRProgram
parseProg input = do
  toks <- tokenize input
  top  <- parseTop toks
  decodeDocument top

decodeDocument :: Datum -> Either String IRProgram
decodeDocument (Lst [Atom "troupe-ir-sexp", verD, progD]) = do
  ver <- decodeInteger verD
  if ver == formatVersion
    then decodeProg progD
    else Left ("unsupported troupe-ir-sexp version: " ++ show ver
               ++ " (this build supports version " ++ show formatVersion ++ ")")
decodeDocument (Lst (Atom "troupe-ir-sexp" : _)) =
  Left "malformed troupe-ir-sexp wrapper: expected (troupe-ir-sexp VERSION PROGRAM)"
decodeDocument d =
  Left ("not a troupe-ir-sexp document: expected head symbol \"troupe-ir-sexp\", got "
        ++ headHint d)

decodeProg :: Datum -> Either String IRProgram
decodeProg (Lst (Atom "program" : atomsD : funDs)) = do
  atoms <- decodeAtoms atomsD
  funs  <- mapM decodeFun funDs
  Right (IRProgram atoms (map noLoc funs))
decodeProg d = Left ("expected (program ...), got " ++ headHint d)

decodeAtoms :: Datum -> Either String C.Atoms
decodeAtoms (Lst (Atom "atoms" : ns)) = C.Atoms <$> mapM asName ns
decodeAtoms d = Left ("expected (atoms ...), got " ++ headHint d)

decodeFun :: Datum -> Either String FunDef
decodeFun (Lst [Atom "fun", nameD, argD, constsD, bodyD]) = do
  name   <- asName nameD
  arg    <- decodeArg argD
  consts <- decodeConsts constsD
  body   <- decodeBB bodyD
  Right (FunDef (HFN name) (noLoc (VN arg)) consts body)
decodeFun d = Left ("expected (fun NAME (arg NAME) CONSTS BODY), got " ++ headHint d)

decodeArg :: Datum -> Either String String
decodeArg (Lst [Atom "arg", nD]) = asName nD
decodeArg d = Left ("expected (arg NAME), got " ++ headHint d)

decodeConsts :: Datum -> Either String Consts
decodeConsts (Lst (Atom "consts" : ps)) = mapM decodeConst ps
decodeConsts d = Left ("expected (consts ...), got " ++ headHint d)

decodeConst :: Datum -> Either String (VarName, C.Lit)
decodeConst (Lst [nD, litD]) = do
  n <- asName nD
  l <- decodeLit litD
  Right (VN n, l)
decodeConst d = Left ("expected (NAME LIT) const binding, got " ++ headHint d)

decodeBB :: Datum -> Either String IRBBTree
decodeBB (Lst [Atom "bb", instsD, termD]) = do
  instDs <- expectList instsD
  insts  <- mapM decodeInst instDs
  term   <- decodeTerm termD
  Right (BB (map noLoc insts) (noLoc term))
decodeBB d = Left ("expected (bb (INST*) TERM), got " ++ headHint d)

decodeInst :: Datum -> Either String IRInst
decodeInst (Lst [Atom "assign", nD, eD]) = do
  n <- asName nD
  e <- decodeExpr eD
  Right (Assign (VN n) e)
decodeInst (Lst [Atom "mkclos", capsD, closD]) = do
  capDs <- expectList capsD
  cloDs <- expectList closD
  caps  <- mapM decodeCap capDs
  clos  <- mapM decodeClo cloDs
  Right (MkFunClosures caps clos)
decodeInst d = Left ("expected (assign ...) or (mkclos ...), got " ++ headHint d)

decodeCap :: Datum -> Either String (VarName, LVarAccess)
decodeCap (Lst [nD, vaD]) = do
  n  <- asName nD
  va <- decodeLVA vaD
  Right (VN n, va)
decodeCap d = Left ("expected (NAME VARACCESS) capture, got " ++ headHint d)

decodeClo :: Datum -> Either String (VarName, HFN)
decodeClo (Lst [nD, hD]) = do
  n <- asName nD
  h <- asName hD
  Right (VN n, HFN h)
decodeClo d = Left ("expected (NAME HFN) closure, got " ++ headHint d)

decodeExpr :: Datum -> Either String IRExpr
decodeExpr (Lst (Atom "bin" : opD : rest)) =
  case rest of
    [aD, bD] -> do op <- decodeBinOp opD
                   a  <- decodeLVA aD
                   b  <- decodeLVA bD
                   Right (Bin op a b)
    _ -> Left "bin expects an operator and two operands"
decodeExpr (Lst [Atom "un", opD, aD]) = do
  op <- decodeUnOp opD
  a  <- decodeLVA aD
  Right (Un op a)
decodeExpr (Lst (Atom "tuple" : vas)) =
  Tuple <$> mapM decodeLVA vas
decodeExpr (Lst (Atom "record" : fields)) =
  Record <$> mapM decodeField fields
decodeExpr (Lst (Atom "with-record" : lvaD : fields)) = do
  lva <- decodeLVA lvaD
  fs  <- mapM decodeField fields
  Right (WithRecord lva fs)
decodeExpr (Lst [Atom "proj-field", lvaD, fD]) = do
  lva <- decodeLVA lvaD
  f   <- asName fD
  Right (ProjField lva f)
decodeExpr (Lst [Atom "proj-idx", lvaD, wD]) = do
  lva <- decodeLVA lvaD
  w   <- decodeWord wD
  Right (ProjIdx lva w)
decodeExpr (Lst (Atom "list" : vas)) =
  List <$> mapM decodeLVA vas
decodeExpr (Lst [Atom "cons", aD, bD]) = do
  a <- decodeLVA aD
  b <- decodeLVA bD
  Right (ListCons a b)
decodeExpr (Lst [Atom "const", litD]) =
  Const <$> decodeLit litD
decodeExpr (Lst [Atom "base", vD]) =
  Base <$> asName vD
decodeExpr (Lst [Atom "lib", lD, vD]) = do
  l <- asName lD
  v <- asName vD
  Right (Lib (LibName l) v)
decodeExpr d = Left ("not a valid expression, got " ++ headHint d)

decodeField :: Datum -> Either String (FieldName, LVarAccess)
decodeField (Lst [nD, vaD]) = do
  n  <- asName nD
  va <- decodeLVA vaD
  Right (n, va)
decodeField d = Left ("expected (NAME VARACCESS) field, got " ++ headHint d)

decodeTerm :: Datum -> Either String IRTerminator
decodeTerm (Lst [Atom "tail-call", fD, aD]) = do
  f <- decodeLVA fD
  a <- decodeLVA aD
  Right (TailCall f a)
decodeTerm (Lst [Atom "ret", aD]) =
  Ret <$> decodeLVA aD
decodeTerm (Lst [Atom "if", cD, tD, eD]) = do
  c <- decodeLVA cD
  t <- decodeBB tD
  e <- decodeBB eD
  Right (If c t e)
decodeTerm (Lst [Atom "assert-else-error", cD, bbD, errD]) = do
  c   <- decodeLVA cD
  bb  <- decodeBB bbD
  err <- decodeLVA errD
  Right (AssertElseError c bb err)
decodeTerm (Lst [Atom "lib-export", aD]) =
  LibExport <$> decodeLVA aD
decodeTerm (Lst [Atom "error", aD]) =
  Error <$> decodeLVA aD
decodeTerm (Lst [Atom "stack-expand", nD, b1D, b2D]) = do
  n  <- asName nD
  b1 <- decodeBB b1D
  b2 <- decodeBB b2D
  Right (StackExpand (VN n) b1 b2)
decodeTerm d = Left ("not a valid terminator, got " ++ headHint d)

decodeLVA :: Datum -> Either String LVarAccess
decodeLVA d = noLoc <$> decodeVA d

decodeVA :: Datum -> Either String VarAccess
decodeVA (Lst [Atom "local", nD]) = (VarLocal . VN) <$> asName nD
decodeVA (Lst [Atom "env", nD])   = (VarEnv . VN)   <$> asName nD
decodeVA (Atom "self")            = Right VarFunSelfRef
decodeVA d = Left ("not a valid variable access, got " ++ headHint d)

decodeLit :: Datum -> Either String C.Lit
decodeLit (Lst [Atom "int", nD])   = (C.LNumeric . C.NumInt) <$> decodeInteger nD
decodeLit (Lst [Atom "float", nD]) = (C.LNumeric . C.NumFloat) <$> decodeDouble nD
decodeLit (Lst [Atom "string", sD]) = C.LString <$> asName sD
decodeLit (Lst [Atom "bool", bD]) = do
  b <- asToken bD
  case b of
    "true"  -> Right (C.LBool True)
    "false" -> Right (C.LBool False)
    _       -> Left ("bad boolean literal: " ++ b)
decodeLit (Atom "unit")            = Right C.LUnit
decodeLit (Lst [Atom "atom", sD])  = C.LAtom <$> asName sD
decodeLit (Lst [Atom "label-string", sD]) = C.LLabel <$> asName sD
decodeLit d@(Lst (Atom "dclabel" : _)) = C.LDCLabel <$> decodeDCLabel d
decodeLit d = Left ("not a valid literal, got " ++ headHint d)

decodeDCLabel :: Datum -> Either String DCLabelExp
decodeDCLabel (Lst [Atom "dclabel", c1, c2]) = do
  a <- decodeComponent c1
  b <- decodeComponent c2
  Right (DCLabelExp (a, b))
decodeDCLabel d = Left ("expected (dclabel COMPONENT COMPONENT), got " ++ headHint d)

decodeComponent :: Datum -> Either String LabelComponent
decodeComponent (Atom "#true")  = Right (ConstComponent LabelTrue)
decodeComponent (Atom "#false") = Right (ConstComponent LabelFalse)
decodeComponent d               = ExprComponent <$> decodeLabelExp d

decodeLabelExp :: Datum -> Either String LabelExp
decodeLabelExp (Lst [Atom "tag", sD]) = TagExp <$> asName sD
decodeLabelExp (Lst (Atom "and" : args)) = decodeNary Conj args
decodeLabelExp (Lst (Atom "or"  : args)) = decodeNary Disj args
decodeLabelExp d = Left ("not a valid label expression, got " ++ headHint d)

-- | Decode an @and@/@or@ form. Accepts n-ary sugar (right-nested); the
-- printer only ever emits the binary form.
decodeNary :: LabelOp -> [Datum] -> Either String LabelExp
decodeNary op ds = do
  es <- mapM decodeLabelExp ds
  case es of
    (x : y : rest) -> Right (foldr1 (OpExp op) (x : y : rest))
    _              -> Left "and/or requires at least two operands"

------------------------------------------------------------
-- Small decoder helpers.
------------------------------------------------------------

-- | A name may be written as a quoted string (the printer always does) or,
-- tolerantly, as a bare symbol.
asName :: Datum -> Either String String
asName (Str s)  = Right s
asName (Atom s) = Right s
asName d        = Left ("expected a name (string or symbol), got " ++ headHint d)

-- | A bare token (symbol/number); tolerantly also a quoted string.
asToken :: Datum -> Either String String
asToken (Atom s) = Right s
asToken (Str s)  = Right s
asToken d        = Left ("expected a token, got " ++ headHint d)

expectList :: Datum -> Either String [Datum]
expectList (Lst ds) = Right ds
expectList d        = Left ("expected a list, got " ++ headHint d)

decodeInteger :: Datum -> Either String Integer
decodeInteger d = do
  s <- asToken d
  case readMaybe s of
    Just i  -> Right i
    Nothing -> Left ("bad integer literal: " ++ s)

decodeDouble :: Datum -> Either String Double
decodeDouble d = do
  s <- asToken d
  case readMaybe s of
    Just x  -> Right x
    Nothing -> Left ("bad float literal: " ++ s)

decodeWord :: Datum -> Either String Word
decodeWord d = do
  i <- decodeInteger d
  if i < 0
    then Left ("ProjIdx index must be non-negative: " ++ show i)
    else if i > maxProjIdx
      then Left ("ProjIdx index exceeds maximum (" ++ show maxProjIdx ++ "): " ++ show i)
      else Right (fromInteger i)

decodeBinOp :: Datum -> Either String BinOp
decodeBinOp d = do
  s <- asToken d
  case lookup s binOpTable of
    Just op -> Right op
    Nothing -> Left ("unknown binary operator: " ++ s)

decodeUnOp :: Datum -> Either String UnaryOp
decodeUnOp d = do
  s <- asToken d
  case lookup s unOpTable of
    Just op -> Right op
    Nothing -> Left ("unknown unary operator: " ++ s)

-- | A short hint naming the head of a datum, for error messages.
headHint :: Datum -> String
headHint (Atom s)         = "symbol " ++ show s
headHint (Str s)          = "string " ++ show s
headHint (Lst [])         = "()"
headHint (Lst (Atom h : _)) = "(" ++ h ++ " ...)"
headHint (Lst _)          = "(...)"

------------------------------------------------------------
-- Position erasure (for stating R1 over position-erased ASTs).
------------------------------------------------------------

-- | Normalize every source position in a program to 'NoPos'. The parser
-- always fills 'NoPos'; erasing the printer's input lets R1 be checked with
-- the derived structural equality.
erasePosProg :: IRProgram -> IRProgram
erasePosProg (IRProgram atoms funs) =
  IRProgram atoms (map (\(Loc _ f) -> noLoc (erasePosFun f)) funs)

erasePosFun :: FunDef -> FunDef
erasePosFun (FunDef hfn (Loc _ vn) consts bb) =
  FunDef hfn (noLoc vn) consts (erasePosBB bb)

erasePosBB :: IRBBTree -> IRBBTree
erasePosBB (BB insts term) =
  BB (map (\(Loc _ i) -> noLoc (erasePosInst i)) insts)
     (case term of Loc _ t -> noLoc (erasePosTerm t))

erasePosInst :: IRInst -> IRInst
erasePosInst (Assign v e) = Assign v (erasePosExpr e)
erasePosInst (MkFunClosures caps clos) =
  MkFunClosures (map (\(v, lva) -> (v, eLVA lva)) caps) clos

erasePosExpr :: IRExpr -> IRExpr
erasePosExpr (Bin op a b)         = Bin op (eLVA a) (eLVA b)
erasePosExpr (Un op a)            = Un op (eLVA a)
erasePosExpr (Tuple xs)           = Tuple (map eLVA xs)
erasePosExpr (Record fs)          = Record (map eField fs)
erasePosExpr (WithRecord lva fs)  = WithRecord (eLVA lva) (map eField fs)
erasePosExpr (ProjField lva f)    = ProjField (eLVA lva) f
erasePosExpr (ProjIdx lva w)      = ProjIdx (eLVA lva) w
erasePosExpr (List xs)            = List (map eLVA xs)
erasePosExpr (ListCons a b)       = ListCons (eLVA a) (eLVA b)
erasePosExpr e@(Const _)          = e
erasePosExpr e@(Base _)           = e
erasePosExpr e@(Lib _ _)          = e

eField :: (FieldName, LVarAccess) -> (FieldName, LVarAccess)
eField (f, lva) = (f, eLVA lva)

erasePosTerm :: IRTerminator -> IRTerminator
erasePosTerm (TailCall f a)            = TailCall (eLVA f) (eLVA a)
erasePosTerm (Ret a)                   = Ret (eLVA a)
erasePosTerm (If c t e)                = If (eLVA c) (erasePosBB t) (erasePosBB e)
erasePosTerm (AssertElseError c bb er) = AssertElseError (eLVA c) (erasePosBB bb) (eLVA er)
erasePosTerm (LibExport a)             = LibExport (eLVA a)
erasePosTerm (Error a)                 = Error (eLVA a)
erasePosTerm (StackExpand v b1 b2)     = StackExpand v (erasePosBB b1) (erasePosBB b2)

eLVA :: LVarAccess -> LVarAccess
eLVA (Loc _ va) = noLoc va
