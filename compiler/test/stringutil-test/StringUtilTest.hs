-- | Tests of 'Util.StringUtil'. The first group pins the exact behaviour of 'splitOn',
-- which replaced MissingH's @Data.List.Utils.split@.
--
-- These are not incidental cases. 'ProcessImports.checkModulePath' rejects a
-- module path with an empty segment by testing @any null@ over the pieces, so
-- the empty-piece cases below are what stands between @import "./a//b"@ and
-- silent acceptance.
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Util.StringUtil (splitOn, decodeStringLiteral)

main :: IO ()
main = defaultMain $ testGroup "Util.StringUtil" [splitOnTests, decodeStringLiteralTests]

splitOnTests :: TestTree
splitOnTests = testGroup "splitOn"
  [ testCase "the documented MissingH example" $
      splitOn "," "foo,bar,,baz," @?= ["foo", "bar", "", "baz", ""]

  , testCase "a multi-character delimiter" $
      splitOn "ba" ",foo,bar,,baz," @?= [",foo,", "r,,", "z,"]

  , testCase "an empty input gives no pieces, not one empty piece" $
      splitOn "," "" @?= []

  , testCase "no delimiter present" $
      splitOn "," "foo" @?= ["foo"]

  , testCase "a leading delimiter gives a leading empty piece" $
      splitOn "/" "/a" @?= ["", "a"]

  , testCase "a trailing delimiter gives a trailing empty piece" $
      splitOn "/" "a/" @?= ["a", ""]

  , testCase "consecutive delimiters give an empty piece between them" $
      splitOn "/" "./a//b" @?= [".", "a", "", "b"]

  , testCase "the input is nothing but one delimiter" $
      splitOn "/" "/" @?= ["", ""]

  , testCase "splitOn is not limited to strings" $
      splitOn [0 :: Int] [1, 0, 2, 0, 0, 3] @?= [[1], [2], [], [3]]
  ]

-- | The argument is the text of a literal as the lexer stores it, so a Haskell
-- @"\\x41"@ here is the four characters a Troupe program writes as @\x41@.
decodeStringLiteralTests :: TestTree
decodeStringLiteralTests = testGroup "decodeStringLiteral"
  [ testCase "text without a backslash denotes its own characters" $
      decodeStringLiteral "Ab" @?= Just [65, 98]

  , testCase "a hexadecimal escape and the plain character denote the same string" $
      decodeStringLiteral "\\x41" @?= decodeStringLiteral "A"

  , testCase "the two spellings of a double quote agree" $
      decodeStringLiteral "\\\"" @?= decodeStringLiteral "\\x22"

  , testCase "the single-character escapes" $
      decodeStringLiteral "\\n\\t\\r\\\\\\'" @?= Just [10, 9, 13, 92, 39]

  , testCase "a four-digit escape is one code unit" $
      decodeStringLiteral "\\u2500" @?= Just [0x2500]

  , testCase "a character beyond the basic plane is its surrogate pair" $
      decodeStringLiteral "\x1F600" @?= decodeStringLiteral "\\uD83D\\uDE00"

  , testCase "an escape outside the interpreted set gives no answer" $
      map decodeStringLiteral ["\\q", "\\0", "\\b", "\\u{41}", "a\\"] @?= replicate 5 Nothing

  , testCase "a hexadecimal escape with too few digits gives no answer" $
      map decodeStringLiteral ["\\x4", "\\x4g", "\\u004"] @?= replicate 3 Nothing
  ]
