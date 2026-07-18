module ProcessImports (processImports) where
import Basics
import Direct
import Control.Monad (unless)
import System.Environment
import System.Exit
import System.Directory (doesFileExist)
import Data.String.Utils
import Data.List (partition)

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
  -- The .exports file carries one value name per line, followed by zero or
  -- more @datatype <group-hash> <canonical-form>@ lines (normalization.md §10).
  -- Value names feed value-name scoping (Core); datatype lines feed the
  -- syntactic-variant resolver (SynVarFolding) and are kept separate here so
  -- they never leak into the value namespace.
  let (dtLines, nameLines) = partition (startswith "datatype ") (lines input)
      datatypes = map parseDatatypeLine dtLines
  -- Validate selective imports if specified. Selection restricts *value*
  -- imports only; datatypes are imported wholesale regardless (they are
  -- compile-time only), so selection is checked against the value names.
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` nameLines) selected
      if null missing
        then return imp { importExports = Just nameLines, importDatatypes = datatypes }
        else die $ "Library '" ++ lib ++ "' does not export: " ++ unwords missing
    Nothing -> return imp { importExports = Just nameLines, importDatatypes = datatypes }

-- | Parse a @datatype <group-hash> <canonical-form>@ interface line into its
-- (hash, canonical-form) pair. The hash is the first whitespace-delimited token
-- after the keyword; the canonical form is the remainder (it contains spaces).
parseDatatypeLine :: String -> (String, String)
parseDatatypeLine line =
  let rest = drop (length ("datatype " :: String)) line
      (h, canon) = break (== ' ') rest
  in (h, dropWhile (== ' ') canon)


processImports' :: Imports -> IO Imports
processImports' (Imports imports)=
  Imports <$> mapM processImport imports


processImports :: Prog -> IO Prog
processImports (Prog imports groups term) = do
  imports' <- processImports' imports
  return $ Prog imports' groups term
