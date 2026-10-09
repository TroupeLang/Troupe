-- | The one list helper the compiler took from MissingH that base has no
-- equivalent for.
--
-- MissingH's @startswith@ and @endswith@ are its own names for
-- 'Data.List.isPrefixOf' and 'Data.List.isSuffixOf' (same argument order), so
-- the call sites use those directly and nothing here reproduces them.
module Util.StringUtil (splitOn, decodeStringLiteral) where

import Data.Char (digitToInt, isHexDigit, ord)
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

-- | The UTF-16 code units of the string that the text of a string literal
-- denotes at run time, or 'Nothing' when the text has a backslash sequence
-- other than the ones below.
--
-- A literal reaches the generated JavaScript as its source text, so JavaScript
-- gives the escapes their meaning. This function reproduces that meaning for
-- @\n@, @\t@, @\r@, @\\@, @\"@, @\'@, @\xHH@ and @\uHHHH@ only.
--
-- > decodeStringLiteral "\\x41" == Just [65]
-- > decodeStringLiteral "A"     == Just [65]
-- > decodeStringLiteral "\\q"   == Nothing
decodeStringLiteral :: String -> Maybe [Int]
decodeStringLiteral [] = Just []
decodeStringLiteral ('\\' : rest) = case rest of
    'n'  : cs -> (10 :) <$> decodeStringLiteral cs
    't'  : cs -> (9 :)  <$> decodeStringLiteral cs
    'r'  : cs -> (13 :) <$> decodeStringLiteral cs
    '\\' : cs -> (92 :) <$> decodeStringLiteral cs
    '"'  : cs -> (34 :) <$> decodeStringLiteral cs
    '\'' : cs -> (39 :) <$> decodeStringLiteral cs
    'x'  : cs -> hex 2 cs
    'u'  : cs -> hex 4 cs
    _         -> Nothing
  where
    hex n cs = case splitAt n cs of
        (ds, cs') | length ds == n && all isHexDigit ds ->
            (foldl (\acc d -> acc * 16 + digitToInt d) 0 ds :) <$> decodeStringLiteral cs'
        _ -> Nothing
decodeStringLiteral (c : cs)
    | ord c < 0x10000 = (ord c :) <$> decodeStringLiteral cs
    | otherwise       = let v = ord c - 0x10000
                        in ([0xD800 + v `div` 0x400, 0xDC00 + v `mod` 0x400] ++)
                           <$> decodeStringLiteral cs
