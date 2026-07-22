module Exports where

-- 2018-07-02: 21.09: this moudle may be redundant; AA

-- 2018-07-02: aa: consider renaming it to ProcessExports for
-- consistency w the imports handling module; though on the other hand
-- the exports are handled in many places throughout the compilation
-- pipeline.


import Basics
import Direct
import TroupePositionInfo (Located(..), unLoc)
import Control.Monad.Except
import Data.List (intercalate)
import Data.String.Utils (startswith)

type Exports = [(Basics.VarName, Basics.VarName)]

-- | The prefix marking a datatype line in a @.exports@ interface file. The one
-- definition of the datatype-line format lives here: 'renderDatatypeLine' /
-- 'isDatatypeLine' / 'parseDatatypeLine' are the writer/recognizer/parser that
-- share it, and 'ProcessImports' consumes them so producer and consumer cannot
-- drift.
datatypePrefix :: String
datatypePrefix = "datatype "

-- | Render one interface datatype line from its (group hash, canonical form):
-- @datatype <group-hash> <canonical-form>@. Inverse of 'parseDatatypeLine'.
renderDatatypeLine :: (String, String) -> String
renderDatatypeLine (h, c) = datatypePrefix ++ h ++ " " ++ c

-- | Whether an interface line is a datatype line (as opposed to a value-name
-- line).
isDatatypeLine :: String -> Bool
isDatatypeLine = startswith datatypePrefix

-- | Parse a datatype interface line into its (group hash, canonical form) pair.
-- Inverse of 'renderDatatypeLine': the hash is the first whitespace-delimited
-- token after the prefix; the canonical form is the remainder (it contains
-- spaces).
parseDatatypeLine :: String -> (String, String)
parseDatatypeLine line =
  let rest = drop (length datatypePrefix) line
      (h, canon) = break (== ' ') rest
  in (h, dropWhile (== ' ') canon)

-- | Assemble the @.exports@ interface content: one value name per line,
-- followed by one datatype line per exported datatype group in declaration
-- order (spec §10). A library exports all its header datatype groups.
exportsFileContent :: [Basics.VarName] -> [(String, String)] -> String
exportsFileContent names groups =
  intercalate "\n" (names ++ map renderDatatypeLine groups)

-- | The @--datatype-hashes@ diagnostic report: one line per datatype group
-- declared in a file, its content hash followed by its canonical form, in
-- declaration order. The hash is the group's global identity (compare it across
-- files or nodes to see whether two declarations agree); the canonical form
-- shows the structure the hash commits to (why two differing hashes differ).
datatypeHashReport :: [(String, String)] -> String
datatypeHashReport groups = unlines [ h ++ "  " ++ c | (h, c) <- groups ]

-- | Extract the main term from let bindings (now works with LTerm)
extractMain :: LTerm -> LTerm
extractMain (Loc _ (Let _ term)) = extractMain term
extractMain x = x

errorMessage = "parse error: libraries need to use restricted syntax for their main body"


extractExports :: Prog -> Except String [String]
extractExports (Prog imports groups term) = do
  case unLoc (extractMain term) of
    List exports -> reify exports
    _ -> throwError errorMessage


reify :: [LTerm] -> Except String [String]
reify = mapM checkOne


checkOne :: LTerm -> Except String String
checkOne (Loc _ (Tuple [Loc _ (Lit (LString s)), Loc _ (Var vn)] _)) = return s
checkOne _ = throwError errorMessage
