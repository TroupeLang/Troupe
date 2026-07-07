-- A standalone executable for testing the dc label and other integrity
-- related components of Troupe 2, 2025-05-13
--
-- With no arguments it prints a few worked CNF conversions (the original
-- demo behavior). With the `--judge` flag it acts as the Haskell side of the
-- differential lattice harness (Step 3): it reads JSON lines
-- `{"x": [[..]], "y": [[..]]}` from stdin and, for each, emits one verdict
-- line comparing `cnfImplies`/`cnfEq` on the two CNFs. The verdict format is a
-- fixed, canonical token line so it can be `diff`ed byte-for-byte against the
-- TypeScript judge (rt/src/proptests/tools/judge.mts).
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main (main) where

import DCLabels
import Data.Aeson (FromJSON(..), withObject, (.:), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)

(\/) = OpExp Disj
(/\) = OpExp Conj

t = TagExp

labexp01 :: LabelExp
labexp01 = (t "alice") \/  ( (t "bob") /\ t "dorothy" ) \/ (t "charlie")
labexp02 = (t "alice") /\ (t "bob") /\ (t "charlie")
labexp03 = (t "alice") /\ ((t "bob") \/ (t "charlie"))

-- One differential case: two CNFs given as clause lists of principal strings.
data Case = Case { caseX :: [[String]], caseY :: [[String]] }

instance FromJSON Case where
  parseJSON = withObject "Case" $ \o -> Case <$> o .: "x" <*> o .: "y"

-- Canonical verdict line. Keep this format and key order identical to the
-- TypeScript judge.
verdict :: BL.ByteString -> BL.ByteString
verdict line =
  case decode line of
    Just (Case xs ys) ->
      let cx = mkCNF xs
          cy = mkCNF ys
      in BL.pack ("implies=" ++ b (cnfImplies cx cy)
                  ++ " equals=" ++ b (cnfEq cx cy))
    Nothing -> BL.pack "PARSE_ERROR"
  where
    b True  = "t"
    b False = "f"

judge :: IO ()
judge = do
  contents <- BL.getContents
  let ls = filter (not . BL.null) (BL.lines contents)
  mapM_ (BL.putStrLn . verdict) ls

demo :: IO ()
demo = do
  print (labexp01)
  print (labelExpToCNF labexp01)
  print (labexp02)
  print (labelExpToCNF labexp02)
  print (labexp03)
  print (labelExpToCNF labexp03)

main :: IO ()
main = do
  args <- getArgs
  if "--judge" `elem` args then judge else demo
