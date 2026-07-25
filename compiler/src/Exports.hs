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
import Data.List (intercalate, partition)
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

-- | The prefix marking a module's own content-addressed identity line in a
-- @.exports@ interface. A module artifact carries exactly one such line (its
-- own IR hash); libraries carry none. It is distinct from the datatype-group
-- hashes ('datatypePrefix'), which serve datatype identity and must not be
-- conflated with module identity. The consumer picks this up in
-- 'ProcessImports.processModuleImport' to resolve @import "./B"@ to B's hash.
moduleHashPrefix :: String
moduleHashPrefix = "module-hash "

-- | Render a module's identity line from its IR hash: @module-hash <hash>@.
renderModuleHashLine :: String -> String
renderModuleHashLine h = moduleHashPrefix ++ h

-- | Whether an interface line is the module-hash line.
isModuleHashLine :: String -> Bool
isModuleHashLine = startswith moduleHashPrefix

-- | Parse the module-hash line into its hash (the token after the prefix).
parseModuleHashLine :: String -> String
parseModuleHashLine = dropWhile (== ' ') . drop (length moduleHashPrefix)

-- | A parsed @.exports@ interface: the module-hash lines (none for a library,
-- exactly one for a module artifact — the caller enforces the arity so its
-- error can name the importing context), the exported value names, and the
-- datatype-group lines. This is the one reader of the interface format; every
-- consumer goes through it, so the writers above and the readers cannot drift.
data ExportsInterface = ExportsInterface
  { eiModuleHashes :: [String]
  , eiNames        :: [Basics.VarName]
  , eiDatatypes    :: [(String, String)]
  }

-- | Parse @.exports@ file content. Total: every line is classified by its
-- prefix; unprefixed lines are value names.
parseExportsFile :: String -> ExportsInterface
parseExportsFile input =
  let (mhLines, rest)      = partition isModuleHashLine (lines input)
      (dtLines, nameLines) = partition isDatatypeLine rest
  in ExportsInterface
       { eiModuleHashes = map parseModuleHashLine mhLines
       , eiNames        = nameLines
       , eiDatatypes    = map parseDatatypeLine dtLines
       }

-- | Assemble the @.exports@ interface content: the module-hash line (present
-- only for module artifacts, whose identity is content-addressed), then one
-- value name per line, then one datatype line per exported datatype group in
-- declaration order (spec §10). A library exports all its header datatype
-- groups and carries no module-hash line.
exportsFileContent :: Maybe String -> [Basics.VarName] -> [(String, String)] -> String
exportsFileContent moduleHash names groups =
  intercalate "\n"
    (maybe [] (\h -> [renderModuleHashLine h]) moduleHash
      ++ names
      ++ map renderDatatypeLine groups)

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
