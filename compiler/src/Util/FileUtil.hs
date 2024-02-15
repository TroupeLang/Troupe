
module Util.FileUtil where

import System.FilePath
import System.Directory

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
