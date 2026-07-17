{-# LANGUAGE RecordWildCards #-}
-- | Parse error formatting with source context display
module ParseError
  ( ParseErrorInfo(..)
  , ParseEnv(..)
  , ParseState(..)
  , initialParseState
  , maxParseErrors
  , minErrorDistance
  , formatParseError
  , formatAllErrors
  , getSourceLine
  , makeCaretLine
  , inferContext
  , suggestFix
  ) where

import Data.List (intercalate)
import Control.Applicative ((<|>))
import Lexer (Token(..), showToken)

-- | Environment for parsing (filename + source text)
data ParseEnv = ParseEnv
  { peFilename :: String
  , peSource   :: String
  } deriving (Eq, Show)

-- | Error accumulation state for multi-error recovery
data ParseState = ParseState
  { psErrors       :: [ParseErrorInfo]  -- ^ Accumulated errors (reverse order)
  , psErrorCount   :: Int               -- ^ For limiting
  , psLastErrorPos :: Maybe (Int, Int)  -- ^ For duplicate suppression (line, col)
  }

-- | Initial parse state with no errors
initialParseState :: ParseState
initialParseState = ParseState [] 0 Nothing

-- | Maximum number of parse errors to report before giving up
maxParseErrors :: Int
maxParseErrors = 10

-- | Minimum line distance between errors to avoid duplicate reporting
minErrorDistance :: Int
minErrorDistance = 2

-- | Complete information about a parse error
data ParseErrorInfo = ParseErrorInfo
  { peiFilename    :: String       -- ^ Source file name
  , peiLine        :: Int          -- ^ Line number (1-indexed)
  , peiColumn      :: Int          -- ^ Column number (1-indexed)
  , peiToken       :: Maybe Token  -- ^ The unexpected token (Nothing for EOF)
  , peiExpected    :: [String]     -- ^ List of expected token names
  , peiSourceLines :: [String]     -- ^ All source lines for context
  , peiContext     :: Maybe String -- ^ Parse context (e.g., "let expression")
  }

-- | Extract a source line (1-indexed)
getSourceLine :: [String] -> Int -> Maybe String
getSourceLine sourceLines lineNum
  | lineNum > 0 && lineNum <= length sourceLines = Just (sourceLines !! (lineNum - 1))
  | otherwise = Nothing

-- | Create a caret line pointing to the error column (1-indexed)
makeCaretLine :: Int -> String
makeCaretLine col = replicate (col - 1) ' ' ++ "^"

-- | Tokens that can start an expression
-- When many of these are expected, we summarize as "expression" instead of listing all
expressionStartTokens :: [String]
expressionStartTokens =
  [ "keyword 'let'", "keyword 'if'", "keyword 'case'"
  , "keyword 'fn'", "keyword 'hn'"
  , "'true'", "'false'"
  , "number", "bigint literal", "float", "string", "identifier", "label"
  , "'('", "'['", "'{'"
  , "'-'"
  , "'`<' (DC label)"
  , "'isTuple'", "'isList'", "'isRecord'", "'not'"
  ]

-- | Check if a token name (string) is an expression starter
isExpressionStartName :: String -> Bool
isExpressionStartName = (`elem` expressionStartTokens)

-- | Format expected tokens, summarizing long expression lists
-- When 8+ expression-starting tokens are expected, summarize as "expression"
formatExpectedTokens :: [String] -> String
formatExpectedTokens [] = ""
formatExpectedTokens [e] = "  expected " ++ e
formatExpectedTokens tokens
  | exprCount >= 8 =
      let others = filter (not . isExpressionStartName) tokens
      in case others of
           [] -> "  expected expression"
           [x] -> "  expected expression or " ++ x
           xs -> "  expected expression or one of: " ++ intercalate ", " xs
  | otherwise =
      "  expected one of: " ++ intercalate ", " tokens
  where
    exprCount = length $ filter isExpressionStartName tokens

-- | Format a complete error message with source context
formatParseError :: ParseErrorInfo -> String
formatParseError info@ParseErrorInfo{..} = unlines $ filter (not . null)
  [ locationLine
  , contextLine
  , ""
  , sourceLine
  , caretLine
  , ""
  , unexpectedLine
  , expectedLine
  , suggestionSection
  ]
  where
    -- Location header: "filename:line:col: parse error"
    locationLine = prefix ++ show peiLine ++ ":" ++ show peiColumn ++ ": parse error"
    prefix = if null peiFilename then "" else peiFilename ++ ":"

    -- Context line: "  (while parsing let expression)"
    -- Auto-infer context if not explicitly provided
    contextLine = case peiContext <|> inferContext peiExpected peiToken of
      Just ctx -> "  (while parsing " ++ ctx ++ ")"
      Nothing  -> ""

    -- Source line with line number: "  5 | in  val _ = ..."
    sourceLine = case getSourceLine peiSourceLines peiLine of
      Just line -> "  " ++ lineNumStr ++ " | " ++ expandTabs line
      Nothing   -> ""

    lineNumStr = show peiLine
    lineNumWidth = length lineNumStr

    -- Caret line pointing to error column
    caretLine = case getSourceLine peiSourceLines peiLine of
      Just line ->
        let tabAdjustedCol = adjustForTabs line peiColumn
        in  "  " ++ replicate lineNumWidth ' ' ++ " | " ++ makeCaretLine tabAdjustedCol
      Nothing -> ""

    -- "unexpected keyword 'val'" or "unexpected end of input"
    unexpectedLine = case peiToken of
      Just tok -> "  unexpected " ++ showToken tok
      Nothing  -> "  unexpected end of input"

    -- "expected expression" or "expected one of: ..." (summarized for long lists)
    expectedLine = formatExpectedTokens peiExpected

    -- Suggestion for common mistakes
    suggestionSection = case suggestFix info of
      Just suggestion -> "\nSuggestion: " ++ suggestion
      Nothing         -> ""

-- | Format multiple errors into a combined output message
formatAllErrors :: [ParseErrorInfo] -> String
formatAllErrors [] = ""
formatAllErrors [e] = formatParseError e
formatAllErrors errs =
  "Found " ++ show (length errs) ++ " parse errors:\n\n" ++
  intercalate "\n----------------------------------------\n\n" (map formatParseError errs)

-- | Expand tabs to spaces (4 spaces per tab, standard)
expandTabs :: String -> String
expandTabs = concatMap expandTab
  where
    expandTab '\t' = "    "
    expandTab c    = [c]

-- | Adjust column position to account for tabs before the error position
adjustForTabs :: String -> Int -> Int
adjustForTabs line col = go 1 1 line
  where
    go srcCol displayCol [] = displayCol
    go srcCol displayCol (c:cs)
      | srcCol >= col = displayCol
      | c == '\t'     = go (srcCol + 1) (displayCol + 4) cs
      | otherwise     = go (srcCol + 1) (displayCol + 1) cs

-- | Infer parsing context from expected tokens
-- This provides "while parsing X" hints without requiring a full context stack
-- Order matters: more specific contexts should come before general ones
inferContext :: [String] -> Maybe Token -> Maybe String
inferContext expected maybeToken = firstJust
  [ checkValInExprContext  -- 'val' where expression expected (common mistake)
  , checkIfContext         -- Check if/then/else first (more specific than pattern)
  , checkRaiseContext      -- raise...to
  , checkCaseContext       -- case...of with | and =>
  , checkLetContext        -- let declarations
  , checkDCLabelContext    -- DC labels
  , checkRecordContext     -- record literals
  , checkListContext       -- list literals
  , checkFunctionContext   -- fn => (general)
  , checkPatternContext    -- patterns (most general, check last)
  ]
  where
    firstJust = foldr (<|>) Nothing

    -- 'val' or 'fun' appearing where expression is expected (after 'in')
    -- This is the most common mistake: forgetting 'end' before 'in'
    checkValInExprContext
      | Just tok <- maybeToken
      , isValOrFun tok
      , "keyword 'let'" `elem` expected  -- Expecting expression start
      , "keyword 'val'" `notElem` expected  -- But val is not expected
      , "keyword 'fun'" `notElem` expected  -- And fun is not expected
      = Just "let expression body"
      | otherwise = Nothing

    isValOrFun TokenVal = True
    isValOrFun TokenFun = True
    isValOrFun _ = False

    -- In if expression (expecting then/else)
    checkIfContext
      | "keyword 'then'" `elem` expected = Just "if expression"
      | "keyword 'else'" `elem` expected = Just "if expression"
      | otherwise = Nothing

    -- In raise expression
    checkRaiseContext
      | "keyword 'to'" `elem` expected = Just "raise expression"
      | otherwise = Nothing

    -- In case expression (expecting | or =>)
    checkCaseContext
      | "'|'" `elem` expected, "'=>'" `elem` expected = Just "case expression"
      | otherwise = Nothing

    -- In let declarations (expecting val/fun/end)
    checkLetContext
      | any (`elem` expected) ["keyword 'val'", "keyword 'fun'", "keyword 'end'"]
      , "keyword 'in'" `notElem` expected = Just "let declarations"
      | "keyword 'end'" `elem` expected
      , "keyword 'in'" `elem` expected = Just "let expression"
      | otherwise = Nothing

    -- In DC label (expecting >` or only ';' for separator)
    checkDCLabelContext
      | dcLabelEnd `elem` expected = Just "DC label"
      -- When only ';' is expected, it's likely inside a DC label
      | expected == ["';'"] = Just "DC label"
      | otherwise = Nothing
      where
        dcLabelEnd = "'>`' (DC label end)"

    -- In record (expecting } or ,)
    checkRecordContext
      | "'}'" `elem` expected, "','" `elem` expected
      , "'='" `elem` expected = Just "record"
      | otherwise = Nothing

    -- In list (expecting ] or ,)
    checkListContext
      | "']'" `elem` expected, "','" `elem` expected
      , "'::'" `notElem` expected = Just "list"
      | otherwise = Nothing

    -- In function definition (expecting =>)
    checkFunctionContext
      | "'=>'" `elem` expected = Just "function or pattern match"
      | otherwise = Nothing

    -- In pattern (expecting = after pattern) - most general, check last
    checkPatternContext
      | "'='" `elem` expected
      , "keyword 'val'" `notElem` expected
      , "keyword 'fun'" `notElem` expected
      , "keyword 'then'" `notElem` expected  -- Not if expression
      , "keyword 'else'" `notElem` expected  -- Not if expression
      = Just "pattern"
      | otherwise = Nothing

-- -----------------------------------------------------------------------------
-- Phase 5: Suggestions for Common Mistakes
-- -----------------------------------------------------------------------------

-- | Helper predicates for token types
isTokenVal :: Token -> Bool
isTokenVal TokenVal = True
isTokenVal _ = False

isTokenFun :: Token -> Bool
isTokenFun TokenFun = True
isTokenFun _ = False

isExpressionStart :: Token -> Bool
isExpressionStart (TokenNum _)    = True
isExpressionStart (TokenFloat _)  = True
isExpressionStart (TokenString _) = True
isExpressionStart (TokenSym _)    = True
isExpressionStart TokenTrue       = True
isExpressionStart TokenFalse      = True
isExpressionStart TokenLParen     = True
isExpressionStart TokenLBracket   = True
isExpressionStart TokenLBrace     = True
isExpressionStart TokenFn         = True
isExpressionStart TokenHn         = True
isExpressionStart TokenIf         = True
isExpressionStart TokenCase       = True
isExpressionStart TokenLet        = True
isExpressionStart _               = False

-- | Suggest fixes for common mistakes
-- Returns a suggestion message if the error matches a known pattern
suggestFix :: ParseErrorInfo -> Maybe String
suggestFix ParseErrorInfo{..} = firstJust
  [ checkValOutsideLet
  , checkFunOutsideLet
  , checkMissingEnd
  , checkMissingIn
  , checkMissingArrow
  , checkDCLabelWrongBrackets  -- Check for <...;...> instead of `<...;...>`
  , checkDCLabelSyntax      -- Check DC label before general semicolon check
  , checkMissingSemicolon
  , checkMismatchedParens
  , checkMismatchedBrackets
  , checkMismatchedBraces
  , checkMissingThen
  , checkMissingElse
  , checkMissingOf
  ]
  where
    firstJust = foldr (<|>) Nothing

    -- 'val' appearing outside let declarations
    checkValOutsideLet
      | Just tok <- peiToken
      , isTokenVal tok
      , "keyword 'end'" `notElem` peiExpected
      , "keyword 'val'" `notElem` peiExpected
      , any isExprLikeExpected peiExpected =
          Just $ "'val' can only appear in let declarations (before 'in').\n" ++
                 "  Either move this declaration before 'in', or just write the expression directly."
      | otherwise = Nothing

    -- 'fun' appearing outside let declarations
    checkFunOutsideLet
      | Just tok <- peiToken
      , isTokenFun tok
      , "keyword 'end'" `notElem` peiExpected
      , "keyword 'fun'" `notElem` peiExpected
      , any isExprLikeExpected peiExpected =
          Just $ "'fun' can only appear in let declarations.\n" ++
                 "  Perhaps you meant 'let fun f x = ... in ... end'?\n" ++
                 "  Or you may have forgotten 'end' before 'in'."
      | otherwise = Nothing

    -- Check if an expected token suggests expression context
    isExprLikeExpected exp =
      exp `elem` ["identifier", "number", "'('", "keyword 'let'",
                  "keyword 'if'", "keyword 'case'", "keyword 'fn'"]

    -- Missing 'end' keyword
    checkMissingEnd
      | "keyword 'end'" `elem` peiExpected
      , length peiExpected <= 5 =
          Just "Missing 'end' keyword?\n  Let and case expressions require 'end' to close them."
      | otherwise = Nothing

    -- Missing 'in' keyword
    checkMissingIn
      | "keyword 'in'" `elem` peiExpected
      , length peiExpected <= 5 =
          Just "Missing 'in' keyword?\n  Syntax: let <declarations> in <expression> end"
      | otherwise = Nothing

    -- Missing '=>' in pattern match
    checkMissingArrow
      | "'=>'" `elem` peiExpected
      , length peiExpected <= 3 =
          Just "Missing '=>' after pattern?\n  Syntax: fn pattern => expression"
      | otherwise = Nothing

    -- Missing semicolon between expressions
    checkMissingSemicolon
      | Just tok <- peiToken
      , isExpressionStart tok
      , any (`elem` peiExpected) ["';'", "keyword 'end'", "keyword 'in'"]
      , length peiExpected <= 10 =
          Just "Missing semicolon?\n  Use ';' to separate sequential expressions."
      | otherwise = Nothing

    -- Mismatched parentheses
    checkMismatchedParens
      | "')'" `elem` peiExpected, length peiExpected == 1 =
          Just "Mismatched parentheses - missing ')'?"
      | "')'" `elem` peiExpected, length peiExpected <= 3 =
          Just "Possibly mismatched parentheses - check for missing ')'."
      | otherwise = Nothing

    -- Mismatched brackets
    checkMismatchedBrackets
      | "']'" `elem` peiExpected, length peiExpected == 1 =
          Just "Mismatched brackets - missing ']'?"
      | "']'" `elem` peiExpected, length peiExpected <= 3 =
          Just "Possibly mismatched brackets - check for missing ']'."
      | otherwise = Nothing

    -- Mismatched braces
    checkMismatchedBraces
      | "'}'" `elem` peiExpected, length peiExpected == 1 =
          Just "Mismatched braces - missing '}'?"
      | "'}'" `elem` peiExpected, length peiExpected <= 3 =
          Just "Possibly mismatched braces - check for missing '}'."
      | otherwise = Nothing

    -- DC label with wrong brackets: <...;...> instead of `<...;...>`
    -- This catches a common mistake where users forget the backticks
    checkDCLabelWrongBrackets
      | Just TokenLt <- peiToken  -- Unexpected '<' token
      , "'`<' (DC label)" `elem` peiExpected  -- DC label start was expected
      = Just $ "DC labels require backticks around the angle brackets.\n" ++
               "  Use `< ... >` not < ... >\n" ++
               "  Example: `< alice ; bob >`"
      | Just TokenLt <- peiToken  -- Unexpected '<' token
      , any isExprLikeExpected peiExpected  -- Expression was expected
      , looksLikeDCLabel =
          Just $ "DC labels require backticks around the angle brackets.\n" ++
                 "  Use `< ... >` not < ... >\n" ++
                 "  Example: `< alice ; bob >`"
      | otherwise = Nothing

    -- Check if the source line at error position looks like a DC label
    -- Pattern: <...;...> or <...> starting from error column
    looksLikeDCLabel =
      case getSourceLine peiSourceLines peiLine of
        Just line ->
          let fromCol = drop (peiColumn - 1) line
          in  startsWithDCLabelPattern fromCol
        Nothing -> False

    -- Check if a string starts with something that looks like <...;...> or <...>
    startsWithDCLabelPattern :: String -> Bool
    startsWithDCLabelPattern s = case s of
      '<':rest -> hasMatchingClose rest 0
      _ -> False

    -- Look for matching '>' with optional ';' inside, tracking nested angle brackets
    hasMatchingClose :: String -> Int -> Bool
    hasMatchingClose [] _ = False
    hasMatchingClose ('>':_) 0 = True  -- Found matching close at depth 0
    hasMatchingClose ('>':rest) n = hasMatchingClose rest (n - 1)  -- Close nested
    hasMatchingClose ('<':rest) n = hasMatchingClose rest (n + 1)  -- Open nested
    hasMatchingClose (_:rest) n = hasMatchingClose rest n

    -- DC label syntax help - Phase 6: specialized DC label messages
    checkDCLabelSyntax
      -- Integrity token in confidentiality position (wrong order)
      | Just tok <- peiToken
      , isIntegrityToken tok
      , peiExpected == ["';'"] =
          Just $ "Integrity component in wrong position.\n" ++
                 "  In DC labels, confidentiality comes BEFORE the semicolon,\n" ++
                 "  and integrity comes AFTER.\n" ++
                 "  Syntax: `< confidentiality ; integrity >`\n" ++
                 "  You have an integrity component (" ++ showToken tok ++ ") in the\n" ++
                 "  confidentiality position. Perhaps you meant to swap them?"
      -- Confidentiality token in integrity position (wrong order)
      | Just tok <- peiToken
      , isConfidentialityToken tok
      , "'>`' (DC label end)" `elem` peiExpected =
          Just $ "Confidentiality component in wrong position.\n" ++
                 "  In DC labels, confidentiality comes BEFORE the semicolon,\n" ++
                 "  and integrity comes AFTER.\n" ++
                 "  Syntax: `< confidentiality ; integrity >`\n" ++
                 "  You have a confidentiality component (" ++ showToken tok ++ ") in the\n" ++
                 "  integrity position. Perhaps you meant to swap them?"
      -- Empty DC label (`<>`)
      | Just tok <- peiToken
      , isDCLabelEnd tok
      , peiExpected == ["';'"] =
          Just $ "Empty DC label is not allowed.\n" ++
                 "  DC labels must have both confidentiality and integrity components.\n" ++
                 "  Syntax: `< confidentiality ; integrity >`\n" ++
                 "  Examples:\n" ++
                 "    `< alice ; bob >`                               -- principal-based\n" ++
                 "    `< #null-confidentiality ; #null-integrity >`   -- public data\n" ++
                 "    `< #root-confidentiality ; #root-integrity >`   -- secret data"
      -- Multiple semicolons
      | Just TokenSemi <- peiToken
      , "'>`' (DC label end)" `elem` peiExpected
      , length peiExpected <= 3 =
          Just $ "Too many semicolons in DC label.\n" ++
                 "  DC labels have exactly ONE semicolon separating confidentiality\n" ++
                 "  and integrity components.\n" ++
                 "  Syntax: `< confidentiality ; integrity >`"
      -- General DC label end expected
      | "'>`' (DC label end)" `elem` peiExpected =
          Just $ "DC label syntax: `< confidentiality ; integrity >`\n" ++
                 "  Example: `< alice ; bob >`"
      -- Only semicolon expected (inside DC label, before semicolon)
      | peiExpected == ["';'"] =
          Just $ "In DC labels, use ';' to separate confidentiality and integrity.\n" ++
                 "  Syntax: `< confidentiality ; integrity >`\n" ++
                 "  Example: `< alice ; bob >`"
      | otherwise = Nothing

    -- Missing 'then' in if expression
    checkMissingThen
      | "keyword 'then'" `elem` peiExpected
      , length peiExpected <= 3 =
          Just "Missing 'then' keyword?\n  Syntax: if <condition> then <expr> else <expr>"
      | otherwise = Nothing

    -- Missing 'else' in if expression
    checkMissingElse
      | "keyword 'else'" `elem` peiExpected
      , length peiExpected <= 3 =
          Just "Missing 'else' keyword?\n  Syntax: if <condition> then <expr> else <expr>"
      | otherwise = Nothing

    -- Missing 'of' in case expression
    checkMissingOf
      | "keyword 'of'" `elem` peiExpected
      , length peiExpected <= 3 =
          Just "Missing 'of' keyword?\n  Syntax: case <expr> of <pattern> => <expr> | ... end"
      | otherwise = Nothing

-- -----------------------------------------------------------------------------
-- Phase 6: DC Label Token Helpers
-- -----------------------------------------------------------------------------

-- | Check if token is an integrity-specific component
isIntegrityToken :: Token -> Bool
isIntegrityToken TokenDCRootInteg = True
isIntegrityToken TokenDCNullInteg = True
isIntegrityToken _ = False

-- | Check if token is a confidentiality-specific component
isConfidentialityToken :: Token -> Bool
isConfidentialityToken TokenDCRootConf = True
isConfidentialityToken TokenDCNullConf = True
isConfidentialityToken _ = False

-- | Check if token is the DC label end marker
isDCLabelEnd :: Token -> Bool
isDCLabelEnd TokenDCLabelRight = True
isDCLabelEnd _ = False
