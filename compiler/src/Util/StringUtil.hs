-- | The one list helper the compiler took from MissingH that base has no
-- equivalent for.
--
-- MissingH's @startswith@ and @endswith@ are its own names for
-- 'Data.List.isPrefixOf' and 'Data.List.isSuffixOf' (same argument order), so
-- the call sites use those directly and nothing here reproduces them.
module Util.StringUtil (splitOn) where

import Data.List (isPrefixOf)

-- | Split on a delimiter, dropping the delimiter and keeping empty pieces:
--
-- > splitOn "," "foo,bar,,baz,"   == ["foo","bar","","baz",""]
-- > splitOn "ba" ",foo,bar,,baz," == [",foo,","r,,","z,"]
--
-- Splitting an empty list gives no pieces at all, rather than one empty piece:
--
-- > splitOn "," "" == []
--
-- The empty pieces are load-bearing. 'ProcessImports.checkModulePath' rejects a
-- module path containing an empty segment by testing @any null@ over this
-- result, so a version that dropped them would silently accept @"./a//b"@.
--
-- This reproduces @Data.List.Utils.split@ from MissingH 1.6.0.2
-- (@src/Data/List/Utils.hs:172-182@), which is where the compiler got it before
-- that dependency was dropped, down to the two edge cases above.
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (before, remainder) = breakOnDelim str
    in before : case remainder of
         []                -> []
         x | x == delim    -> [[]]
           | otherwise     -> splitOn delim (drop (length delim) x)
  where
    breakOnDelim [] = ([], [])
    breakOnDelim l@(c:cs)
      | delim `isPrefixOf` l = ([], l)
      | otherwise            = let (before, remainder) = breakOnDelim cs
                               in (c : before, remainder)
