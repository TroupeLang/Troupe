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

type Exports = [(Basics.VarName, Basics.VarName)]

-- | Assemble the @.exports@ interface content: one value name per line,
-- followed by one @datatype <group-hash> <canonical-form>@ line per exported
-- datatype group in declaration order (spec §10). A library exports all its
-- header datatype groups.
exportsFileContent :: [Basics.VarName] -> [(String, String)] -> String
exportsFileContent names groups =
  intercalate "\n" (names ++ [ "datatype " ++ h ++ " " ++ c | (h, c) <- groups ])

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
