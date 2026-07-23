{-# LANGUAGE OverloadedStrings #-}

-- | The per-main-program dependencies file: the @path <-> hash <-> name@
-- authority for content-addressed module identity.
--
-- It lives next to the main file as @<main>.deps.json@ and holds one entry per
-- module the program transitively imports. The compiler *enforces* it (reads a
-- dependency's actual hash from its @.exports@ and checks it against the pin
-- here); the maintenance utility *establishes* it (records actual hashes as
-- pins). The runtime seeds its @hash -> (location, name)@ resolver from it. See
-- @_dev_planning/module-system/content-addressed-identity.md@ (§3, §5, §9).
module DepsFile
  ( DepEntry(..)
  , depsFilePath
  , readDepsFile
  , writeDepsFile
  , renderDepsFile
  , lookupPinByPath
  ) where

import           Data.Aeson            (FromJSON (..), withObject, (.:), eitherDecode')
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (find, sortOn)
import           System.Directory      (doesFileExist)
import           System.FilePath       (replaceExtension)
import           Util.FileUtil         (atomicWriteFileD)

-- | One pinned dependency: its root-relative source path (the resolution key),
-- its content hash (bare base32hex), and its user-visible name (today always
-- equal to the path; the field is the seat for a future friendlier alias).
data DepEntry = DepEntry
  { depPath :: String
  , depHash :: String
  , depName :: String
  } deriving (Eq, Show)

instance FromJSON DepEntry where
  parseJSON = withObject "DepEntry" $ \o ->
    DepEntry <$> o .: "path" <*> o .: "hash" <*> o .: "name"

-- | The document wrapper: @{ "deps": [ ... ] }@.
newtype DepsDoc = DepsDoc [DepEntry]

instance FromJSON DepsDoc where
  parseJSON = withObject "deps file" $ \o -> DepsDoc <$> o .: "deps"

-- | The dependencies file for a main program: @<main>.deps.json@, next to the
-- main file.
depsFilePath :: FilePath -> FilePath
depsFilePath mainFile = replaceExtension mainFile "deps.json"

-- | Read the dependencies file. @Nothing@ if it does not exist; @Just (Left e)@
-- if it exists but is malformed; @Just (Right es)@ on success.
readDepsFile :: FilePath -> IO (Maybe (Either String [DepEntry]))
readDepsFile path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      bytes <- BL.readFile path
      case eitherDecode' bytes of
        Left err            -> return (Just (Left err))
        Right (DepsDoc es)  -> return (Just (Right es))

-- | Look up a pinned dependency by its root-relative path.
lookupPinByPath :: String -> [DepEntry] -> Maybe DepEntry
lookupPinByPath p = find ((== p) . depPath)

-- | Render the dependencies file: a stable, human-readable JSON object with
-- entries sorted by path and a fixed key order, so a re-established file diffs
-- minimally. Hand-rendered (rather than via a JSON pretty-printer dependency)
-- for full control over the byte layout.
renderDepsFile :: [DepEntry] -> String
renderDepsFile entries =
  "{\n  \"deps\": [" ++ body ++ "]\n}\n"
  where
    sorted = sortOn depPath entries
    body
      | null sorted = ""
      | otherwise   = "\n" ++ commaSep (map renderEntry sorted) ++ "\n  "
    commaSep = foldr1 (\a b -> a ++ ",\n" ++ b)
    renderEntry (DepEntry p h n) =
      "    {\"path\": " ++ jsonStr p
        ++ ", \"hash\": " ++ jsonStr h
        ++ ", \"name\": " ++ jsonStr n ++ "}"

-- | JSON-encode a string literal. Paths, hashes, and names are ASCII with no
-- control characters, so only the two structural characters need escaping.
jsonStr :: String -> String
jsonStr s = '"' : concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c    = [c]

-- | Write the dependencies file (atomically, so a concurrent enforcing compile
-- never reads a torn pin set).
writeDepsFile :: FilePath -> [DepEntry] -> IO ()
writeDepsFile path entries = atomicWriteFileD path (renderDepsFile entries)
