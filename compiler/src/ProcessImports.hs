module ProcessImports (PinCheck(..), processImports, discoverModules) where
import Basics
import Direct
import DepsFile (DepEntry(..), lookupPinByPath)
import Exports (isDatatypeLine, parseDatatypeLine, isModuleHashLine, parseModuleHashLine)
import Parser (parseProg)
import Control.Monad (unless, foldM)
import System.Environment
import System.Exit
import System.Directory (doesFileExist)
import System.FilePath
import Data.List (partition, intercalate)
import Data.String.Utils

-- | Whether the frontend enforces the dependencies file (a normal compile) or
-- establishes it (the maintenance utility). Under 'Enforce', a dependency's
-- actual hash (read from its @.exports@) is checked against its pin and a
-- missing or mismatched pin is fatal; under 'Establish', no check is made and
-- the resolved actuals are returned for the utility to record. See
-- @_dev_planning/module-system/content-addressed-identity.md@ §5.
data PinCheck = Enforce [DepEntry] | Establish

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

-- | Check the literal path of a module import: must start with "./" or "../",
-- must not end in "/", and every segment must be nonempty. With identity
-- decoupled from location (content-addressed hashes), a "../" path is a pure
-- resolution concern — the path never becomes identity — so the descendant-only
-- restriction is gone; the remaining requirement (resolves to a real .trp file)
-- is checked at resolution.
checkModulePath :: String -> Either String ()
checkModulePath p
  | not (startswith "./" p || startswith "../" p)
                     = Left "the path must start with \"./\" or \"../\""
  | endswith "/" p   = Left "the path must not end with \"/\""
  | any null segs    = Left "the path must not contain empty segments"
  | otherwise        = Right ()
  where segs = split "/" p

-- | Lexically collapse @seg/..@ and @.@ from a path, without touching the
-- filesystem (so it never resolves symlinks or requires the file to exist).
-- 'System.FilePath.normalise' does not drop @..@; this does, so the same file
-- reached via different spellings canonicalizes to one path and one module. A
-- leading @..@ (the path escapes above its base) is preserved.
collapseDotDot :: FilePath -> FilePath
collapseDotDot p = joinPath (go [] (splitDirectories p))
  where
    go acc []                    = reverse acc
    go acc ("." : rest)          = go acc rest
    go (top : acc) (".." : rest)
      | top /= ".."              = go acc rest          -- pop a real segment
    go acc (".." : rest)         = go (".." : acc) rest -- leading ".." stays
    go acc (seg : rest)          = go (seg : acc) rest

-- | Resolve a module import literal against the importing file's directory.
-- Returns (source file path, root-relative key without extension). Only a "./"
-- prefix is stripped; a "../" is kept so resolution walks up from the importing
-- file's directory. Two canonicalizations, each of a different form:
--
--   * the source file path is 'collapseDotDot'-canonicalized, so different
--     spellings of one file dedup to a single module in 'discoverModules' and
--     one compiled artifact;
--   * the key is @makeRelative@ against the *uncollapsed* target (so it comes
--     out relative to the program root, keeping any leading "..") and then
--     'collapseDotDot'-cleaned of interior "seg/.." — the runtime joins it onto
--     the root, so it must stay root-relative.
--
-- The key is a resolution/display path (it may begin with "..") — never the
-- identity, which is the content hash.
resolveModule :: FilePath -> FilePath -> String -> (FilePath, String)
resolveModule root importingFile lit =
  let rel       = if startswith "./" lit then drop 2 lit else lit
      rawTarget = normalise (takeDirectory importingFile </> rel)
      target    = collapseDotDot rawTarget
      key       = collapseDotDot (makeRelative root rawTarget)
  in (target ++ ".trp", key)

-- | Root-relative display name of a file, for diagnostics.
displayPath :: FilePath -> FilePath -> String
displayPath root f = makeRelative root f

processModuleImport :: PinCheck -> FilePath -> FilePath -> ImportDecl -> IO (ImportDecl, DepEntry)
processModuleImport pin root file imp = do
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
  -- A module's .exports has the same shape as a library's: one value name per
  -- line and zero or more @datatype ...@ lines, plus (for a module) its own
  -- @module-hash <hash>@ line. Strip the module-hash line first, then partition
  -- the rest exactly as processLibImport does, so a module's datatype interface
  -- reaches the syntactic-variant resolver (importer-side constructor
  -- resolution) and neither the datatype lines nor the module-hash line leak
  -- into the value namespace. Selection restricts value imports only.
  let (mhLines, rest)      = partition isModuleHashLine (lines input)
      (dtLines, nameLines) = partition isDatatypeLine rest
      datatypes            = map parseDatatypeLine dtLines
  actualHash <- case mhLines of
    [l] -> return (parseModuleHashLine l)
    []  -> die $ "module " ++ show lit ++ " imported from " ++ displayPath root file
               ++ " has no content hash in " ++ displayPath root expFile
               ++ " (recompile it)"
    _   -> die $ "module " ++ show lit ++ " has multiple content-hash lines in "
               ++ displayPath root expFile
  -- The user-visible name is, today, the root-relative path; the dependencies
  -- file carries it as its own field so runtime diagnostics stay human-readable.
  let entry = DepEntry { depPath = key, depHash = actualHash, depName = key }
  -- Structural interface check first (does the module export the selected
  -- names?), so a malformed program reports its own error regardless of the
  -- pin state.
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` nameLines) selected
      unless (null missing) $
        die $ "Module " ++ show lit ++ " does not export: " ++ unwords missing
    Nothing -> return ()
  -- Then enforce the pin (a normal compile) or record the actual (the utility).
  -- The integrity point: the frontend never links a dependency whose content
  -- diverges from what the project pinned. A missing pin (no dependencies file,
  -- or an entry absent from it) is a compile error pointing at the utility.
  case pin of
    Establish -> return ()
    Enforce pins -> case lookupPinByPath key pins of
      Nothing ->
        die $ "no pin for module " ++ show lit ++ " (" ++ key ++ ") imported from "
            ++ displayPath root file
            ++ "; run 'troupec --update-deps' on the main program to establish it"
      Just p
        | depHash p == actualHash -> return ()
        | otherwise ->
            die $ "module " ++ show lit ++ " (" ++ key ++ ") imported from "
                ++ displayPath root file ++ " has content hash " ++ actualHash
                ++ " but the dependencies file pins " ++ depHash p
                ++ "; re-run 'troupec --update-deps' if this change is intended"
  -- Replace the stored path with the module's content hash; codegen and the
  -- runtime address the module as "module:<hash>".
  return ( imp { importExports = Just nameLines
               , importDatatypes = datatypes
               , importPath = Just actualHash }
         , entry )

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
  -- The .exports file carries one value name per line, followed by zero or
  -- more @datatype <group-hash> <canonical-form>@ lines (normalization.md §10).
  -- Value names feed value-name scoping (Core); datatype lines feed the
  -- syntactic-variant resolver (SynVarFolding) and are kept separate here so
  -- they never leak into the value namespace.
  let (dtLines, nameLines) = partition isDatatypeLine (lines input)
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

--------------------------------------------------------------------------------

-- | Process one import, returning the resolved declaration and, for a module
-- import, the dependency entry it resolved (its @(path, hash, name)@); a library
-- import yields 'Nothing'.
processImport :: PinCheck -> FilePath -> FilePath -> ImportDecl -> IO (ImportDecl, Maybe DepEntry)
processImport pin root file imp =
  case importPath imp of
    Just _  -> do (imp', entry) <- processModuleImport pin root file imp
                  return (imp', Just entry)
    Nothing -> do imp' <- processLibImport imp
                  return (imp', Nothing)

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
-- the main file). Returns the program with resolved imports and the module
-- dependency entries it resolved (the module @(path, hash, name)@ triples,
-- for the maintenance utility to record; ignored on a normal enforcing compile).
processImports :: PinCheck -> FilePath -> FilePath -> Prog -> IO (Prog, [DepEntry])
processImports pin root file (Prog (Imports imports) groups term) = do
  checkDuplicateBinds imports
  results <- mapM (processImport pin root file) imports
  let imports' = map fst results
      deps     = [ d | (_, Just d) <- results ]
  return (Prog (Imports imports') groups term, deps)

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
