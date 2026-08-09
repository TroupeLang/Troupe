module ProcessImports (PinCheck(..), processImports, discoverModules) where
import Basics
import Surface
import DepsFile (DepEntry(..), lookupPinByPath)
import Exports (ExportsInterface(..), parseExportsFile)
import Parser (parseProg)
import Control.Monad (unless, foldM)
import System.Environment
import System.Exit
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Set as Set
import Util.StringUtil (splitOn)

-- | Whether the frontend enforces the dependencies file (a normal compile) or
-- establishes it (the maintenance utility). Under 'Enforce', a dependency's
-- actual hash (read from its @.exports@) is checked against its pin and a
-- missing or mismatched pin is fatal; under 'Establish', no check is made and
-- the resolved actuals are returned for the utility to record. See
-- @_dev_planning/module-system/content-addressed-identity.md@ §5.
data PinCheck = Enforce [DepEntry] | Establish

defaultLibFolder="/lib/out/"
defaultFfiFolder="/ffi/"
defaultBin="/bin/troupec"

-- Try to get home from executable path (returns Nothing if not possible)
tryGetRelativeHome :: IO (Maybe String)
tryGetRelativeHome = do
   progPath <- getExecutablePath
   if defaultBin `isSuffixOf` progPath
   then do
       let home = take (length progPath - length defaultBin) progPath
       markerExists <- doesFileExist (home ++ "/.troupe-root")
       if markerExists then return (Just home) else return Nothing
   else return Nothing

tryGetTroupeHome :: IO (Maybe String)
tryGetTroupeHome = do
  -- Try self-location first (for worktree support)
  selfLocated <- tryGetRelativeHome
  case selfLocated of
      Just home -> return (Just home)
      Nothing -> lookupEnv "TROUPE"  -- Fall back to TROUPE env var

getTroupeHome :: IO String
getTroupeHome = do
  maybeHome <- tryGetTroupeHome
  case maybeHome of
      Just home -> return home
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
  | not ("./" `isPrefixOf` p || "../" `isPrefixOf` p)
                     = Left "the path must start with \"./\" or \"../\""
  | "/" `isSuffixOf` p = Left "the path must not end with \"/\""
  | any null segs    = Left "the path must not contain empty segments"
  | otherwise        = Right ()
  where segs = splitOn "/" p

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

-- | Express @target@ relative to @base@, lexically: collapse both, drop their
-- common prefix, and step up out of whatever remains of @base@. Unlike
-- 'System.FilePath.makeRelative', which hands back @target@ unchanged whenever
-- @base@ is not a literal prefix of it, this yields a genuinely relative path
-- with leading ".." segments for a target outside @base@.
--
-- Both arguments must be written against the same directory (both relative to
-- the same working directory, or both absolute) — every call here derives them
-- from one main-program path, so they are. Matching leading ".." segments are
-- common prefix under that precondition: they name the same directory.
relativeToBase :: FilePath -> FilePath -> FilePath
relativeToBase base target =
  joinPath (map (const "..") baseRest ++ targetRest)
  where
    (baseRest, targetRest) = dropCommon (segs base) (segs target)
    segs = splitDirectories . collapseDotDot
    dropCommon (b : bs) (t : ts) | b == t = dropCommon bs ts
    dropCommon bs ts                      = (bs, ts)

-- | Resolve a module import literal against the importing file's directory.
-- Returns (source file path, root-relative key without extension). Only a "./"
-- prefix is stripped; a "../" is kept so resolution walks up from the importing
-- file's directory. Two canonicalizations, each of a different form:
--
--   * the source file path is 'collapseDotDot'-canonicalized, so different
--     spellings of one file dedup to a single module in 'discoverModules' and
--     one compiled artifact;
--   * the key is the target expressed relative to the program root by
--     'relativeToBase', so it keeps or gains leading ".." for a module outside
--     the root and never carries the spelling (absolute, or relative to the
--     working directory) the main-program argument happened to have — the
--     runtime joins the key onto the root, so it must stay root-relative.
--
-- The key is a resolution/display path (it may begin with "..") — never the
-- identity, which is the content hash.
resolveModule :: FilePath -> FilePath -> String -> (FilePath, String)
resolveModule root importingFile lit =
  let rel       = if "./" `isPrefixOf` lit then drop 2 lit else lit
      rawTarget = normalise (takeDirectory importingFile </> rel)
      target    = collapseDotDot rawTarget
      key       = relativeToBase root rawTarget
  in (target ++ ".trp", key)

-- | Root-relative display name of a file, for diagnostics.
displayPath :: FilePath -> FilePath -> String
displayPath root f = makeRelative root f

-- | Fixities an import contributes: restricted to the selection when one is
-- given (a fixity is moot without its value). Qualified imports keep their
-- fixities in the record too — the re-association pass gates on the mode.
restrictFixities :: ImportDecl -> [(VarName, Fixity)] -> [(VarName, Fixity)]
restrictFixities imp fxs = case importSelected imp of
  Just selected -> filter ((`elem` selected) . fst) fxs
  Nothing       -> fxs

-- | Resolve a module import. The literal path is passed in rather than
-- re-extracted from @imp@: 'processImport' is the only caller and it selects
-- this function by matching that very field.
processModuleImport :: PinCheck -> FilePath -> FilePath -> ImportDecl -> String -> IO (ImportDecl, DepEntry)
processModuleImport pin root file imp lit = do
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
  -- @module-hash <hash>@ line. 'parseExportsFile' (Exports) is the one reader
  -- of the format, so a module's datatype interface reaches the
  -- syntactic-variant resolver (importer-side constructor resolution) and
  -- neither the datatype lines nor the module-hash line leak into the value
  -- namespace. Selection restricts value imports only.
  let iface     = parseExportsFile input
      nameLines = eiNames iface
      datatypes = eiDatatypes iface
  actualHash <- case eiModuleHashes iface of
    [h] -> return h
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
               , importFixities = restrictFixities imp (eiFixities iface)
               , importSource = FromModule actualHash }
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
  -- syntactic-variant resolver (SynVarFolding) and are kept separate by the
  -- shared reader so they never leak into the value namespace. (A library
  -- artifact carries no module-hash line; the reader would strip one anyway.)
  let iface     = parseExportsFile input
      nameLines = eiNames iface
      datatypes = eiDatatypes iface
  -- Validate selective imports if specified. Selection restricts *value*
  -- imports only; datatypes are imported wholesale regardless (they are
  -- compile-time only), so selection is checked against the value names.
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` nameLines) selected
      if null missing
        then return imp { importExports = Just nameLines
                        , importDatatypes = datatypes
                        , importFixities = restrictFixities imp (eiFixities iface) }
        else die $ "Library '" ++ lib ++ "' does not export: " ++ unwords missing
    Nothing -> return imp { importExports = Just nameLines
                          , importDatatypes = datatypes
                          , importFixities = restrictFixities imp (eiFixities iface) }

--------------------------------------------------------------------------------
-- Native requires (require native Name)

-- | Resolve a native require against its manifest, @$TROUPE/ffi/<Name>.exports@.
-- A manifest carries value names and fixity lines only; a module-hash or
-- datatype line in one is rejected.
processNativeRequire :: ImportDecl -> IO ImportDecl
processNativeRequire imp = do
  troupeEnv <- getTroupeHome
  let LibName name = importLib imp
  let fname = troupeEnv ++ defaultFfiFolder ++ name ++ ".exports"
  fileExists <- doesFileExist fname
  unless fileExists $
    -- Report the location relative to the Troupe home ($TROUPE) so the
    -- message is stable across checkouts (it is captured in golden tests).
    die $ "unknown native module '" ++ name
        ++ "': no $TROUPE" ++ defaultFfiFolder ++ name ++ ".exports"
  input <- readFile fname
  let iface     = parseExportsFile input
      nameLines = eiNames iface
  unless (null (eiModuleHashes iface)) $
    die $ "invalid native module manifest $TROUPE" ++ defaultFfiFolder ++ name
        ++ ".exports: module-hash lines are not allowed"
  unless (null (eiDatatypes iface)) $
    die $ "invalid native module manifest $TROUPE" ++ defaultFfiFolder ++ name
        ++ ".exports: datatype lines are not allowed"
  case importSelected imp of
    Just selected -> do
      let missing = filter (`notElem` nameLines) selected
      unless (null missing) $
        die $ "native module '" ++ name ++ "' does not export: " ++ unwords missing
    Nothing -> return ()
  return imp { importExports = Just nameLines
             , importFixities = restrictFixities imp (eiFixities iface) }

-- | The value names declared across every installed native-module manifest
-- (@$TROUPE/ffi/*.exports@), read once per compile. The renamer's builtin
-- fallthrough consults this set so that a native name used without its
-- @require@ gets a targeted error naming the missing declaration. When the
-- Troupe home cannot be determined or has no @ffi/@ directory, no manifests
-- are installed and the set is empty.
installedNativeNames :: IO (Set.Set VarName)
installedNativeNames = do
  maybeHome <- tryGetTroupeHome
  case maybeHome of
    Nothing -> return Set.empty
    Just home -> do
      let dir = home ++ defaultFfiFolder
      dirExists <- doesDirectoryExist dir
      if not dirExists
        then return Set.empty
        else do
          files <- listDirectory dir
          let manifests = [ dir </> f | f <- files, takeExtension f == ".exports" ]
          names <- mapM (fmap (eiNames . parseExportsFile) . readFile) manifests
          return (Set.fromList (concat names))

--------------------------------------------------------------------------------

-- | Process one import, returning the resolved declaration and, for a module
-- import, the dependency entry it resolved (its @(path, hash, name)@); a library
-- import or a native require yields 'Nothing'.
processImport :: PinCheck -> FilePath -> FilePath -> ImportDecl -> IO (ImportDecl, Maybe DepEntry)
processImport pin root file imp =
  case importSource imp of
    FromModule lit -> do (imp', entry) <- processModuleImport pin root file imp lit
                         return (imp', Just entry)
    FromLibrary -> do imp' <- processLibImport imp
                      return (imp', Nothing)
    FromNative -> do imp' <- processNativeRequire imp
                     return (imp', Nothing)

-- | The name an import binds (alias if present, otherwise the library name or
-- the module's last path segment).
boundName :: ImportDecl -> String
boundName imp = let LibName n = case importAlias imp of
                                  Just a  -> a
                                  Nothing -> importLib imp
                in n

-- | Reject two imports binding the same name when a module import or a native
-- require is involved (existing behavior for library-only collisions is left
-- unchanged).
checkDuplicateBinds :: [ImportDecl] -> IO ()
checkDuplicateBinds imports = mapM_ checkOne (zip [(0::Int)..] imports)
  where
    checkOne (i, imp) =
      let clashes = [ imp' | (j, imp') <- zip [0..] imports, j < i
                           , boundName imp' == boundName imp
                           , participates imp || participates imp' ]
      in case clashes of
           []      -> return ()
           (_ : _) -> die $ "two imports bind the name '" ++ boundName imp
                          ++ "'; use 'as' to disambiguate"
    participates imp = case importSource imp of
      FromLibrary  -> False
      FromModule _ -> True
      FromNative   -> True

-- | Process the imports of the program in file `file`, with module imports
-- resolved relative to it and displayed relative to `root` (the directory of
-- the main file). Returns the program with resolved imports, the module
-- dependency entries it resolved (the module @(path, hash, name)@ triples,
-- for the maintenance utility to record; ignored on a normal enforcing
-- compile), and the installed native-module names (for the renamer's
-- missing-require error).
processImports :: PinCheck -> FilePath -> FilePath -> Prog -> IO (Prog, [DepEntry], Set.Set VarName)
processImports pin root file (Prog (Imports imports) fixities groups term) = do
  checkDuplicateBinds imports
  results <- mapM (processImport pin root file) imports
  nativeNames <- installedNativeNames
  let imports' = map fst results
      deps     = [ d | (_, Just d) <- results ]
  return (Prog (Imports imports') fixities groups term, deps, nativeNames)

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
            Right (Prog (Imports imports) _ _ _) -> do
              let lits = [ lit | imp <- imports, FromModule lit <- [importSource imp] ]
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
