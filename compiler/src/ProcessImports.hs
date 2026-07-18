module ProcessImports (processImports) where
import Basics
import Direct
import Control.Monad (unless)
import System.Environment
import System.Exit
import System.Directory (doesFileExist)
import Data.String.Utils 

defaultLibFolder="/lib/out/" 
defaultBin="/bin/troupec"

-- Try to get home from executable path (returns Nothing if not possible)
tryGetRelativeHome :: IO (Maybe String)
tryGetRelativeHome = do
   progPath <- getExecutablePath
   if endswith defaultBin progPath
   then do
       let home = take (length progPath - length defaultBin) progPath
       markerExists <- doesFileExist (home ++ "/.troupe-root")
       if markerExists then return (Just home) else return Nothing
   else return Nothing

getTroupeHome :: IO String
getTroupeHome = do
  -- Try self-location first (for worktree support)
  selfLocated <- tryGetRelativeHome
  case selfLocated of
      Just home -> return home
      Nothing -> do
          -- Fall back to TROUPE env var
          maybeVar <- lookupEnv "TROUPE"
          case maybeVar of
              Just troupeEnv -> return troupeEnv
              Nothing -> die "Cannot determine Troupe home folder. Consider setting up the TROUPE environment variable" 
      


processImport :: ImportDecl -> IO ImportDecl
processImport imp = do
  troupeEnv <- getTroupeHome
  let LibName lib = importLib imp
  let fname = troupeEnv ++ defaultLibFolder ++ lib ++ ".exports"
  fileExists <- doesFileExist fname
  unless fileExists $
    -- Report the location relative to the Troupe home ($TROUPE) so the
    -- message is stable across checkouts (it is captured in golden tests).
    die $ "cannot find library '" ++ lib
        ++ "' (looked in $TROUPE" ++ defaultLibFolder ++ lib ++ ".exports)"
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
processImports (Prog imports groups term) = do
  imports' <- processImports' imports
  return $ Prog imports' groups term
