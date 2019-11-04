module ProcessImports (processImports) where
import Basics
import Direct
import System.Environment




processImport (LibName lib, _) = do
  troupeEnv <- getEnv "TROUPE" 
  let fname = troupeEnv ++ "/lib/out/" ++ lib ++ ".exports" 
  input <- readFile fname
  return ( LibName lib, Just (lines input))


processImports' :: Imports -> IO Imports
processImports' (Imports imports)=
  Imports <$> mapM processImport imports


processImports :: Prog -> IO Prog
processImports (Prog imports atoms term) = do
  imports' <- processImports' imports
  return $ Prog imports' atoms term


-- TODO: 2018-07-02: AA: proper error handling in case we have errors
-- loading information from the lib files
