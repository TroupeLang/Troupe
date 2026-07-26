
module Util.FileUtil where

import System.FilePath
import System.Directory
import System.IO (openTempFile, hPutStr, hClose)

-- A thin wrapper around system writeFile that creates a missing
-- directory; this is useful for creating out directories for
-- libraries and when the working project folder is just fetched from
-- the repo; 2018-07-15: AA

writeFileD filename x = do
  let dirpath = takeDirectory filename
  createDirectoryIfMissing False dirpath -- do not create parent dirs
                                         -- because this should never
                                         -- be required in our use
                                         -- cases
  writeFile filename x

-- | Write a file atomically: stream the content to a uniquely-named temporary
-- file in the destination directory, then rename it into place. 'renameFile' is
-- atomic within one filesystem (the temp sits beside the target, so it is), so a
-- concurrent reader sees either the old complete file or the new complete one,
-- never a torn read. Module artifacts (.exports, .js, .deps.json) are read by
-- other compiler and runtime processes that may run while this one writes them
-- (e.g. a build compiling several programs that share a module in parallel), so
-- their writes go through here.
atomicWriteFileD :: FilePath -> String -> IO ()
atomicWriteFileD filename x = do
  let dirpath = takeDirectory filename
  createDirectoryIfMissing False dirpath
  (tmpPath, h) <- openTempFile dirpath (takeFileName filename ++ ".tmp")
  hPutStr h x
  hClose h
  renameFile tmpPath filename
