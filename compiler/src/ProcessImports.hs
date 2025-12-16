module ProcessImports (processImports) where
import Basics
import Direct
import System.Environment
import System.Exit
import Data.String.Utils 

defaultLibFolder="/lib/out/" 
defaultBin="/bin/troupec"

getRelativeHome :: IO String 
getRelativeHome = do 
   progPath <- getExecutablePath
   if endswith defaultBin progPath
   then do 
       let home = take ( length progPath - length defaultBin) progPath 
       return home
   else do
       die "Cannot determine Troupe home folder. Consider setting up the TROUPE environment variable" 

getTroupeHome :: IO String 
getTroupeHome = do 
  maybeVar <- lookupEnv "TROUPE" 
  case maybeVar of 
      Nothing -> getRelativeHome 
      Just troupeEnv  -> return troupeEnv 
      


processImport :: ImportDecl -> IO ImportDecl
processImport imp = do
  troupeEnv <- getTroupeHome
  let LibName lib = importLib imp
  let fname = troupeEnv ++ defaultLibFolder ++ lib ++ ".exports"
  input <- readFile fname
  let exports = lines input
  -- Validate selective imports if specified
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` exports) selected
      if null missing
        then return imp { importExports = Just exports }
        else die $ "Library '" ++ lib ++ "' does not export: " ++ unwords missing
    Nothing -> return imp { importExports = Just exports }


processImports' :: Imports -> IO Imports
processImports' (Imports imports)=
  Imports <$> mapM processImport imports


processImports :: Prog -> IO Prog
processImports (Prog imports atoms term) = do
  imports' <- processImports' imports
  return $ Prog imports' atoms term


-- TODO: 2018-07-02: AA: proper error handling in case we have errors
-- loading information from the lib files
