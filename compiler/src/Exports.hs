module Exports where

-- 2018-07-02: 21.09: this moudle may be redundant; AA

-- 2018-07-02: aa: consider renaming it to ProcessExports for
-- consistency w the imports handling module; though on the other hand
-- the exports are handled in many places throughout the compilation
-- pipeline.


import Basics
import Direct
import Control.Monad.Except

type Exports = [(Basics.VarName, Basics.VarName)]

extractMain :: Term -> Term
extractMain (Let _ term) = extractMain term
extractMain x = x

errorMessage = "parse error: libraries need to use restricted syntax for their main body"


extractExports :: Prog -> Except String [String]
extractExports (Prog imports atoms term) = do
  case extractMain term of
    List exports -> reify exports
    _ -> throwError errorMessage


reify :: [Term] -> Except String [String]
reify = mapM checkOne 


checkOne :: Term -> Except String String
checkOne (Tuple [Lit (LString s), Var vn]) = return s
checkOne _ = throwError errorMessage
