-- | Pins the exact behaviour of 'splitOn', which replaced MissingH's
-- @Data.List.Utils.split@.
--
-- These are not incidental cases. 'ProcessImports.checkModulePath' rejects a
-- module path with an empty segment by testing @any null@ over the pieces, so
-- the empty-piece cases below are what stands between @import "./a//b"@ and
-- silent acceptance.
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Util.StringUtil (splitOn)

main :: IO ()
main = defaultMain $ testGroup "splitOn"
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
