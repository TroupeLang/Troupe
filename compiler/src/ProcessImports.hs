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
      


processImport (LibName lib, _) = do
  troupeEnv <- getTroupeHome
  let fname = troupeEnv ++ defaultLibFolder ++ lib ++ ".exports" 
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
