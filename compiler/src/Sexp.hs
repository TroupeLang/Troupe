-- | The s-expression datum layer and the serialization classes over it.
--
-- Two layers, kept apart on purpose:
--
--   * /Datum/ — the lexical and structural layer: 'Datum', a reader
--     ('readDatum') and a canonical renderer ('renderDatum'). It knows nothing
--     about the Troupe IR.
--   * /Class/ — 'Sexp', carrying both directions, with one hand-written
--     instance per type of the serialized closure. Instances live in the module
--     that defines the type, so there are no orphans and adding a constructor
--     breaks the build at the one instance that must handle it. The two
--     directions are one class, as in cereal's 'Data.Serialize.Serialize', so a
--     type cannot acquire an encoding without a decoding to match it.
--
-- The document layer that wraps an IR program in @(troupe-ir-sexp VERSION …)@,
-- and the format specification it implements, are in "IRSexp"
-- (@compiler/docs/spec-troupe-ir-sexp.md@).
module Sexp
  ( Datum(..)
    -- * Class
  , Sexp(..)
    -- * Text layer
  , renderDatum
  , readDatum
    -- * Decoding helpers
  , asName
  , asToken
  , expectList
  , context
  , headHint
  ) where

import           Data.Char (isSpace, isHexDigit, digitToInt)
import           Numeric (showHex)
import           Text.Read (readMaybe)
import qualified Text.PrettyPrint.HughesPJ as PP

------------------------------------------------------------
-- The datum.
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
-- The classes.
------------------------------------------------------------

-- | A type with an s-expression encoding.
class Sexp a where
  -- | Encode a value as a datum. Total: every value of the type has an
  -- encoding.
  toSexp   :: a -> Datum
  -- | Decode a value from a datum, or explain why the datum is not one.
  fromSexp :: Datum -> Either String a

-- Numeric leaves are written as bare atoms, so that they read back as numbers
-- rather than as strings.

instance Sexp Integer where
  toSexp = Atom . show
  fromSexp d = do
    s <- asToken d
    case readMaybe s of
      Just i  -> Right i
      Nothing -> Left ("bad integer literal: " ++ s)

instance Sexp Double where
  toSexp = Atom . show
  fromSexp d = do
    s <- asToken d
    case readMaybe s of
      Just x  -> Right x
      Nothing -> Left ("bad float literal: " ++ s)

------------------------------------------------------------
-- Decoding helpers.
------------------------------------------------------------

-- | A name may be written as a quoted string (the renderer always does) or,
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

-- | Prefix a decoding failure with where it happened.
context :: String -> Either String a -> Either String a
context what (Left e) = Left (what ++ ": " ++ e)
context _    r        = r

-- | A short hint naming the head of a datum, for error messages.
headHint :: Datum -> String
headHint (Atom s)           = "symbol " ++ show s
headHint (Str s)            = "string " ++ show s
headHint (Lst [])           = "()"
headHint (Lst (Atom h : _)) = "(" ++ h ++ " ...)"
headHint (Lst _)            = "(...)"

------------------------------------------------------------
-- Rendering a datum to text.
------------------------------------------------------------

-- | Render a datum in the canonical layout.
renderDatum :: Datum -> String
renderDatum d = PP.renderStyle sty (ppDatum d)
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
-- Reading text back into a datum.
------------------------------------------------------------

-- | Read a single top-level datum from text. Tolerant lexically: whitespace is
-- insignificant and @;@ runs to end of line.
readDatum :: String -> Either String Datum
readDatum input = tokenize input >>= parseTop

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
