module ProcessImports (processImports, discoverModules) where
import Basics
import Direct
import Parser (parseProg)
import Control.Monad (unless, foldM)
import System.Environment
import System.Exit
import System.Directory (doesFileExist)
import System.FilePath
import Data.List (intercalate)
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

--------------------------------------------------------------------------------
-- Module imports (import "./Path")
--
-- Terminology: a *library* is what a bare-identifier import names (resolved
-- under $TROUPE/lib/out); a *module* is what a quoted-path import names (a
-- program-local .trp file, resolved relative to the importing file).

-- | Check the literal path of a module import: must start with "./", must not
-- contain "..", must not end in "/", and every segment must be nonempty.
checkModulePath :: String -> Either String ()
checkModulePath p
  | not (startswith "./" p) = Left "the path must start with \"./\""
  | endswith "/" p          = Left "the path must not end with \"/\""
  | any (== "..") segs      = Left "the path must not contain \"..\""
  | any null segs           = Left "the path must not contain empty segments"
  | otherwise               = Right ()
  where segs = split "/" (drop 2 p)

-- | Resolve a module import literal against the importing file's directory.
-- Returns (source file path, root-relative key without extension).
-- The key is what codegen and the runtime use, prefixed with "module:".
resolveModule :: FilePath -> FilePath -> String -> (FilePath, String)
resolveModule root importingFile lit =
  let target = normalise (takeDirectory importingFile </> drop 2 lit)
      key    = makeRelative root target
  in (target ++ ".trp", key)

-- | Root-relative display name of a file, for diagnostics.
displayPath :: FilePath -> FilePath -> String
displayPath root f = makeRelative root f

processModuleImport :: FilePath -> FilePath -> ImportDecl -> IO ImportDecl
processModuleImport root file imp = do
  let Just lit = importPath imp
  case checkModulePath lit of
    Left reason -> die $ "invalid module import " ++ show lit ++ " in "
                       ++ displayPath root file ++ ": " ++ reason
    Right () -> return ()
  let (src, key) = resolveModule root file lit
  srcExists <- doesFileExist src
  unless srcExists $
    die $ "cannot find module " ++ show lit ++ " imported from "
        ++ displayPath root file ++ " (no " ++ displayPath root src ++ ")"
  let expFile = takeDirectory src </> "out" </> takeBaseName src <.> "exports"
  expExists <- doesFileExist expFile
  unless expExists $
    die $ "module " ++ show lit ++ " imported from " ++ displayPath root file
        ++ " is not compiled (no " ++ displayPath root expFile ++ ")"
  input <- readFile expFile
  let exports = lines input
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` exports) selected
      unless (null missing) $
        die $ "Module " ++ show lit ++ " does not export: " ++ unwords missing
    Nothing -> return ()
  -- Canonicalize the stored path to the root-relative key; codegen and the
  -- runtime address the module as "module:<key>".
  return imp { importExports = Just exports, importPath = Just key }

--------------------------------------------------------------------------------
-- Library imports (import Lib)

processLibImport :: ImportDecl -> IO ImportDecl
processLibImport imp = do
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

--------------------------------------------------------------------------------

processImport :: FilePath -> FilePath -> ImportDecl -> IO ImportDecl
processImport root file imp =
  case importPath imp of
    Just _  -> processModuleImport root file imp
    Nothing -> processLibImport imp

-- | The name an import binds (alias if present, otherwise the library name or
-- the module's last path segment).
boundName :: ImportDecl -> String
boundName imp = let LibName n = case importAlias imp of
                                  Just a  -> a
                                  Nothing -> importLib imp
                in n

-- | Reject two imports binding the same name when a module import is involved
-- (existing behavior for library-only collisions is left unchanged).
checkDuplicateBinds :: [ImportDecl] -> IO ()
checkDuplicateBinds imports = mapM_ checkOne (zip [(0::Int)..] imports)
  where
    checkOne (i, imp) =
      let clashes = [ imp' | (j, imp') <- zip [0..] imports, j < i
                           , boundName imp' == boundName imp
                           , isModule imp || isModule imp' ]
      in case clashes of
           []      -> return ()
           (_ : _) -> die $ "two imports bind the name '" ++ boundName imp
                          ++ "'; use 'as' to disambiguate"
    isModule imp = case importPath imp of Just _ -> True; Nothing -> False

-- | Process the imports of the program in file `file`, with module imports
-- resolved relative to it and displayed relative to `root` (the directory of
-- the main file).
processImports :: FilePath -> FilePath -> Prog -> IO Prog
processImports root file (Prog (Imports imports) atoms term) = do
  checkDuplicateBinds imports
  imports' <- mapM (processImport root file) imports
  return $ Prog (Imports imports') atoms term

--------------------------------------------------------------------------------
-- Module graph discovery for the compilation driver.

-- | Starting from the main file, follow module imports and return the module
-- source files in dependency order (dependencies first, main file excluded).
-- Cycles and unresolvable imports are fatal.
discoverModules :: FilePath -> IO [FilePath]
discoverModules mainFile = do
  let root = takeDirectory mainFile
  (order, _) <- visit root [] ([], []) mainFile
  return (reverse (filter (/= mainFile) order))
  where
    -- `stack` is the chain of files currently being visited (for cycle
    -- display); the accumulator is (postorder, done).
    visit root stack (order, done) file
      | file `elem` done = return (order, done)
      | file `elem` stack =
          let names = map (displayPath root) (dropWhile (/= file) (reverse stack) ++ [file])
          in die $ "module import cycle: " ++ intercalate " -> " names
      | otherwise = do
          exists <- doesFileExist file
          unless exists $
            die $ "cannot find module file " ++ displayPath root file
          input <- readFile file
          case parseProg file input of
            Left err -> die err
            Right (Prog (Imports imports) _ _) -> do
              let lits = [ lit | imp <- imports, Just lit <- [importPath imp] ]
              deps <- mapM (resolveOne root file) lits
              (order', done') <- foldM (visit root (file : stack)) (order, done) deps
              return (file : order', file : done')
    resolveOne root file lit = do
      case checkModulePath lit of
        Left reason -> die $ "invalid module import " ++ show lit ++ " in "
                           ++ displayPath root file ++ ": " ++ reason
        Right () -> return ()
      let (src, _) = resolveModule root file lit
      srcExists <- doesFileExist src
      unless srcExists $
        die $ "cannot find module " ++ show lit ++ " imported from "
            ++ displayPath root file ++ " (no " ++ displayPath root src ++ ")"
      return src
