{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact-vector acceptance tests for 'SynVarHash'.
--
-- Each vector checks canonical-string equality and/or hash equality against the
-- worked examples in @_dev_planning/syntactic-variants/normalization.md@
-- (section 6, section 10). Several vectors feed datatypes and constructors in
-- UNSORTED order to prove that canonicalization sorting happens inside the
-- module.
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           SynVarHash

-- Exact group hashes from the spec, reused as dependency hashes in later
-- vectors.
hBinop :: String
hBinop = "lbf8abh66bb9uir0juna1l3pvvidvdsnhj3151va3ddhh470tia0"

hExpr :: String
hExpr = "52lkqeu14qslt1b0fak7l0v3bheji43a44qoci30j035eq7v37b0"

-- 6.1 Option: singleton group, one type parameter. Constructors fed SOME before
-- NONE (unsorted) to prove sorting.
optionGroup :: Group
optionGroup =
  [ ("option", 1, [ ("SOME", Just (Var 0)), ("NONE", Nothing) ]) ]

optionCanon :: String
optionCanon = "(group (dt option 1 (ctor NONE) (ctor SOME (var 0))))"

optionHash :: String
optionHash = "0vqdmrf0i23aeh3nej9red8nmd2a60384vu9ahgj50bqbok2bt90"

-- 6.3 binop: singleton group, no params. Constructors fed SUB before ADD.
binopGroup :: Group
binopGroup =
  [ ("binop", 0, [ ("SUB", Nothing), ("ADD", Nothing) ]) ]

binopCanon :: String
binopCanon = "(group (dt binop 0 (ctor ADD) (ctor SUB)))"

-- 6.3 expr: depends on binop by hash. Constructors fed LIT before BIN.
exprGroup :: Group
exprGroup =
  [ ("expr", 0
    , [ ("LIT", Just (Prim "int"))
      , ("BIN", Just (Prod [ Ext hBinop "binop", In "expr", In "expr" ]))
      ])
  ]

exprCanon :: String
exprCanon =
  "(group (dt expr 0 (ctor BIN (prod (ext "
    ++ hBinop ++ " binop) (in expr) (in expr))) (ctor LIT (prim int))))"

-- 6.4 mutually recursive group. Datatypes fed stmt before expr, constructors
-- shuffled, to prove member and constructor sorting.
mutualGroup :: Group
mutualGroup =
  [ ("stmt", 0
    , [ ("SEQ",    Just (Prod [ In "stmt", In "stmt" ]))
      , ("SKIP",   Nothing)
      , ("ASSIGN", Just (Prod [ Prim "string", In "expr" ]))
      ])
  , ("expr", 0
    , [ ("PAIR", Just (Prod [ In "expr", In "expr" ]))
      , ("DO",   Just (In "stmt"))
      , ("LIT",  Just (Prim "int"))
      ])
  ]

mutualCanon :: String
mutualCanon =
  "(group (dt expr 0 (ctor DO (in stmt)) (ctor LIT (prim int)) "
    ++ "(ctor PAIR (prod (in expr) (in expr)))) "
    ++ "(dt stmt 0 (ctor ASSIGN (prod (prim string) (in expr))) "
    ++ "(ctor SEQ (prod (in stmt) (in stmt))) (ctor SKIP)))"

mutualHash :: String
mutualHash = "n7rtb872v5kc9phus9s24hf5a0olokcmsqkl16jlbbvvbd4hut6g"

-- 10 cmd: client datatype referencing the imported expr group by hash.
cmdGroup :: Group
cmdGroup =
  [ ("cmd", 0
    , [ ("SKIP", Nothing)
      , ("EVAL", Just (Ext hExpr "expr"))
      ])
  ]

cmdCanon :: String
cmdCanon =
  "(group (dt cmd 0 (ctor EVAL (ext " ++ hExpr ++ " expr)) (ctor SKIP)))"

cmdHash :: String
cmdHash = "b33sbuuols9nlngu253qhh2vur7o6qc89ovnh0ui114oij6jasog"

main :: IO ()
main = defaultMain $ testGroup "SynVarHash exact vectors"
  [ testGroup "6.1 option"
    [ testCase "canonical" $ canonicalGroup optionGroup @?= optionCanon
    , testCase "hash"      $ groupHash optionGroup @?= optionHash
    ]
  , testGroup "6.3 binop"
    [ testCase "canonical" $ canonicalGroup binopGroup @?= binopCanon
    , testCase "hash"      $ groupHash binopGroup @?= hBinop
    ]
  , testGroup "6.3 expr"
    [ testCase "canonical" $ canonicalGroup exprGroup @?= exprCanon
    , testCase "hash"      $ groupHash exprGroup @?= hExpr
    ]
  , testGroup "6.4 mutual recursion"
    [ testCase "canonical" $ canonicalGroup mutualGroup @?= mutualCanon
    , testCase "hash"      $ groupHash mutualGroup @?= mutualHash
    ]
  , testGroup "10 cmd (import reference)"
    [ testCase "canonical" $ canonicalGroup cmdGroup @?= cmdCanon
    , testCase "hash"      $ groupHash cmdGroup @?= cmdHash
    ]
  , testGroup "constructor tags"
    [ testCase "expr#BIN" $
        constructorTag hExpr "expr" "BIN"
          @?= hExpr ++ "#expr#BIN"
    , testCase "binop#ADD" $
        constructorTag hBinop "binop" "ADD"
          @?= hBinop ++ "#binop#ADD"
    ]
  ]
