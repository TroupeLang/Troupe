-- A standalone executable for testing the dc label and other integrity
-- related components of Troupe 2, 2025-05-13
--
-- With no arguments it prints a few worked CNF conversions (the original
-- demo behavior). With the `--judge` flag it acts as the Haskell side of the
-- differential lattice harness: it reads JSON lines from stdin and, for each,
-- emits one canonical verdict line so it can be `diff`ed byte-for-byte against
-- the TypeScript judge (rt/src/proptests/tools/judge.mts).
--
-- Two case kinds are dispatched on a `kind` discriminator:
--
--   * kind:"cnf" (or absent, for legacy Step 3 lines):
--       {"x": [[..]], "y": [[..]]} -- each of x/y a CNF as [[String]].
--       Verdict: `implies=<t|f> equals=<t|f>` via `cnfImplies`/`cnfEq`.
--
--   * kind:"v1" (Step 4c):
--       {"raw": "{ BOB, alice }", "canon": "alice,bob"} -- two V1 label
--       surface strings. Verdict: `v1eq=<t|f>` via `v1LabelEq raw canon`,
--       i.e. whether the two V1 strings denote the same label under the
--       compiler's V1 normalization.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main (main) where

import DCLabels
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)

(\/) = OpExp Disj
(/\) = OpExp Conj

t = TagExp

labexp01 :: LabelExp
labexp01 = (t "alice") \/  ( (t "bob") /\ t "dorothy" ) \/ (t "charlie")
labexp02 = (t "alice") /\ (t "bob") /\ (t "charlie")
labexp03 = (t "alice") /\ ((t "bob") \/ (t "charlie"))

-- One differential case. A CNF case carries two CNFs (clause lists of principal
-- strings); a V1 case carries two V1 label surface strings.
data Case
  = CnfCase [[String]] [[String]]
  | V1Case String String

instance FromJSON Case where
  parseJSON = withObject "Case" $ \o -> do
    kind <- o .:? "kind"
    case (kind :: Maybe String) of
      Just "v1" -> V1Case <$> o .: "raw" <*> o .: "canon"
      _         -> CnfCase <$> o .: "x" <*> o .: "y"

-- Canonical verdict line. Keep this format and key order identical to the
-- TypeScript judge.
verdict :: BL.ByteString -> BL.ByteString
verdict line =
  case decode line of
    Just (CnfCase xs ys) ->
      let cx = mkCNF xs
          cy = mkCNF ys
      in BL.pack ("implies=" ++ b (cnfImplies cx cy)
                  ++ " equals=" ++ b (cnfEq cx cy))
    Just (V1Case raw canon) ->
      BL.pack ("v1eq=" ++ b (v1LabelEq raw canon))
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
