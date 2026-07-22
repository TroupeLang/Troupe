{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact-vector acceptance tests for 'SynVarHash'.
--
-- Each vector checks canonical-string equality and/or hash equality against the
-- worked examples in @_dev_planning/syntactic-variants/normalization.md@
-- (section 6, section 10). Several vectors feed datatypes and constructors in
-- UNSORTED order to prove that canonicalization sorting happens inside the
-- module.
module Main (main) where

import           Data.List (isInfixOf, partition)
import           Control.Monad.Except (runExcept)

import           Test.Tasty
import           Test.Tasty.HUnit

import           SynVarHash
import qualified Stack2JS
import qualified Stack
import           CompileMode (CompileMode(..))
import           Exports (exportsFileContent, isDatatypeLine, parseDatatypeLine, datatypeHashReport)
import           SynVarFolding (foldProg)
import           Direct (Prog(..), Term(List))
import           Basics (Imports(..), ImportDecl(..), ImportMode(..), LibName(..))
import           TroupePositionInfo (Located(..), PosInf(..))

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

-- 6.5 box: singleton group, one type parameter, no application.
boxGroup :: Group
boxGroup = [ ("box", 1, [ ("BOX", Just (Var 0)) ]) ]

boxCanon :: String
boxCanon = "(group (dt box 1 (ctor BOX (var 0))))"

hBox :: String
hBox = "jbke14euh6s5nkl5i3isctbnioe150ajvucq2au5d2vhimvijucg"

-- 6.5 item: references box applied to int. Constructors fed T before NOTHING.
itemGroup :: Group
itemGroup =
  [ ("item", 0
    , [ ("T",       Just (App [Prim "int"] (RExt hBox "box")))
      , ("NOTHING", Nothing)
      ])
  ]

itemCanon :: String
itemCanon =
  "(group (dt item 0 (ctor NOTHING) (ctor T (app (prim int) (ext "
    ++ hBox ++ " box)))))"

itemHash :: String
itemHash = "6qnqkn1d01i9vj3u6k15jrq88abg92ou34tanhm4pmtp9d29pe0g"

-- 6.5 intlist: built-in list application uses the uniform target shape.
intlistGroup :: Group
intlistGroup =
  [ ("intlist", 0, [ ("L", Just (App [Prim "int"] (RBuiltin "list"))) ]) ]

intlistCanon :: String
intlistCanon = "(group (dt intlist 0 (ctor L (app (prim int) (builtin list)))))"

intlistHash :: String
intlistHash = "c009kcon60h45fqsui73qlbc4tako7evhnpbjoc2qna6kdmm1p00"

-- 6.5 pair: two type parameters, no application.
pairGroup :: Group
pairGroup = [ ("pair", 2, [ ("P", Just (Prod [ Var 0, Var 1 ])) ]) ]

pairCanon :: String
pairCanon = "(group (dt pair 2 (ctor P (prod (var 0) (var 1)))))"

hPair :: String
hPair = "ja320ve8cs3b94m21eak1evrlfr228jctjgonsv5r25ubtr8lc20"

-- 6.5 pr: multi-argument application to pair, one n-ary node.
prGroup :: Group
prGroup =
  [ ("pr", 0
    , [ ("Q", Just (App [Prim "int", Prim "string"] (RExt hPair "pair"))) ])
  ]

prCanon :: String
prCanon =
  "(group (dt pr 0 (ctor Q (app (prim int) (prim string) (ext "
    ++ hPair ++ " pair)))))"

prHash :: String
prHash = "dlpoemskvn9405qmvla6m5l2c3rkn2rb21b3qn5m7hg836vaunrg"

-- 6.5 tree: a parameterized datatype referencing itself applied.
-- Constructors fed NODE before LEAF.
treeGroup :: Group
treeGroup =
  [ ("tree", 1
    , [ ("NODE", Just (App [App [Var 0] (RIn "tree")] (RBuiltin "list")))
      , ("LEAF", Nothing)
      ])
  ]

treeCanon :: String
treeCanon =
  "(group (dt tree 1 (ctor LEAF) "
    ++ "(ctor NODE (app (app (var 0) (in tree)) (builtin list)))))"

treeHash :: String
treeHash = "l4t3v11o49m7cc0kgbcekj0nb8jml2qtc4bcpif69kgmuiqkl5tg"

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
  , testGroup "6.5 box"
    [ testCase "canonical" $ canonicalGroup boxGroup @?= boxCanon
    , testCase "hash"      $ groupHash boxGroup @?= hBox
    ]
  , testGroup "6.5 item (applied ext reference)"
    [ testCase "canonical" $ canonicalGroup itemGroup @?= itemCanon
    , testCase "hash"      $ groupHash itemGroup @?= itemHash
    ]
  , testGroup "6.5 intlist (built-in application)"
    [ testCase "canonical" $ canonicalGroup intlistGroup @?= intlistCanon
    , testCase "hash"      $ groupHash intlistGroup @?= intlistHash
    ]
  , testGroup "6.5 pair"
    [ testCase "canonical" $ canonicalGroup pairGroup @?= pairCanon
    , testCase "hash"      $ groupHash pairGroup @?= hPair
    ]
  , testGroup "6.5 pr (multi-argument application)"
    [ testCase "canonical" $ canonicalGroup prGroup @?= prCanon
    , testCase "hash"      $ groupHash prGroup @?= prHash
    ]
  , testGroup "6.5 tree (self-application)"
    [ testCase "canonical" $ canonicalGroup treeGroup @?= treeCanon
    , testCase "hash"      $ groupHash treeGroup @?= treeHash
    ]
  , testGroup "constructor tags"
    [ testCase "expr#BIN" $
        constructorTag hExpr "expr" "BIN"
          @?= hExpr ++ "#expr#BIN"
    , testCase "binop#ADD" $
        constructorTag hBinop "binop" "ADD"
          @?= hBinop ++ "#binop#ADD"
    ]
  , testGroup "datatype records embedded in generated JS (spec 10)"
    -- Guards the emission moved into Stack2JS: a regression that stops emitting
    -- the records makes the load-time skew check vacuous but silent, so it must
    -- fail loudly here. Runs the real whole-program codegen entry point on an
    -- empty program carrying only the records.
    [ testCase "library emits its exported hashes" $ do
        let js = genJS Library (Stack2JS.DatatypeRecords [optionHash] [])
        assertBool "missing this.__datatypeHashes" ("this.__datatypeHashes" `isInfixOf` js)
        assertBool "missing the exported hash"     (optionHash `isInfixOf` js)
    , testCase "consuming program emits its consumed record" $ do
        let js = genJS Normal (Stack2JS.DatatypeRecords [] [("VariantsDemo", [optionHash])])
        assertBool "missing this.__consumedDatatypeHashes"
          ("this.__consumedDatatypeHashes" `isInfixOf` js)
        assertBool "missing the consumed library name" ("VariantsDemo" `isInfixOf` js)
        assertBool "missing the consumed hash"         (optionHash `isInfixOf` js)
    , testCase "no records means no record declarations" $ do
        let js = genJS Normal Stack2JS.noDatatypeRecords
        assertBool "unexpected __datatypeHashes"
          (not ("__datatypeHashes" `isInfixOf` js))
        assertBool "unexpected __consumedDatatypeHashes"
          (not ("__consumedDatatypeHashes" `isInfixOf` js))
    ]
  , testGroup "exports interface line format (single definition point)"
    -- The writer (exportsFileContent) and the reader (the partition +
    -- parseDatatypeLine used by ProcessImports) share one datatype-line format
    -- in Exports; this round-trips a names + datatype-lines interface through
    -- both so producer and consumer cannot drift silently.
    [ testCase "names + datatype lines round-trip writer -> parser" $ do
        let names  = ["describe", "unbox", "area"]
            groups = [ (optionHash, optionCanon)
                     , (hBox, boxCanon)
                     , (mutualHash, mutualCanon) ]
            content = exportsFileContent names groups
            (dtLines, nameLines) = partition isDatatypeLine (lines content)
        nameLines               @?= names
        map parseDatatypeLine dtLines @?= groups
    ]
  , testGroup "datatype-hashes diagnostic report format"
    -- The --datatype-hashes flag prints this: one line per group, "<hash>  <canon>"
    -- (two-space separated), in the given order, trailing newline per line.
    [ testCase "one line per group: hash, two spaces, canonical form" $
        datatypeHashReport [(optionHash, optionCanon), (hBinop, binopCanon)]
          @?= optionHash ++ "  " ++ optionCanon ++ "\n"
            ++ hBinop ++ "  " ++ binopCanon ++ "\n"
    , testCase "no datatype groups yields empty output" $
        datatypeHashReport [] @?= ""
    ]
  , testGroup "imported interface checksum (spec 10)"
    -- Identity comes from the recomputed hash; the stored hash is verified
    -- against it on read. A well-formed interface is accepted; a tampered
    -- stored hash fails compilation, naming the library.
    [ testCase "well-formed interface is accepted" $
        case foldImports [(optionHash, optionCanon)] of
          Right _ -> return ()
          Left e  -> assertFailure ("expected acceptance, got: " ++ e)
    , testCase "tampered stored hash is rejected, naming the library" $
        case foldImports [(tamperedHash, optionCanon)] of
          Left e  -> do
            assertBool ("error should name the library, got: " ++ e)
              ("Fake" `isInfixOf` e)
            assertBool ("error should flag corruption, got: " ++ e)
              ("corrupt" `isInfixOf` e)
          Right _ -> assertFailure "expected the tampered interface to be rejected"
    ]
  , testGroup "parse round-trip (parse . render == id on canonical strings)"
    [ roundTrip "option"  optionGroup
    , roundTrip "binop"   binopGroup
    , roundTrip "expr"    exprGroup
    , roundTrip "mutual"  mutualGroup
    , roundTrip "cmd"     cmdGroup
    , roundTrip "box"     boxGroup
    , roundTrip "item"    itemGroup
    , roundTrip "intlist" intlistGroup
    , roundTrip "pair"    pairGroup
    , roundTrip "pr"      prGroup
    , roundTrip "tree"    treeGroup
    ]
  ]

-- | Generate JS for an empty program carrying only the given datatype records,
-- through the real whole-program codegen entry point (source maps off).
genJS :: CompileMode -> Stack2JS.DatatypeRecords -> String
genJS mode records =
  fst (Stack2JS.stack2JSWithMappings mode False False records Nothing
         (Stack.ProgramStackUnit (Stack.StackProgram [])))

-- | Parsing the canonical string and re-rendering must reproduce it exactly,
-- and the hash recomputed from the parsed form must match the original. This
-- pins @parseGroup@ as the inverse of @canonicalGroup@ on canonical input for
-- every worked vector (products, applications, ext references, built-ins,
-- multiple type parameters, self-application, mutual recursion).
roundTrip :: String -> Group -> TestTree
roundTrip name g = testGroup name
  [ testCase "canonical" $
      (canonicalGroup <$> parseGroup canon) @?= Right canon
  , testCase "hash" $
      (groupHash <$> parseGroup canon) @?= Right (groupHash g)
  ]
  where canon = canonicalGroup g

-- | A syntactically well-formed group hash that is not the hash of any group
-- used here, standing in for a corrupted / hand-edited interface line.
tamperedHash :: String
tamperedHash = replicate 52 '0'

-- | Run 'foldProg' on a trivial program that imports one library carrying the
-- given (stored hash, canonical form) datatype interface. Exercises the
-- interface-reading path (buildImportEnv) where the stored hash is verified
-- against the recomputed one.
foldImports :: [(String, String)] -> Either String ()
foldImports dts =
  () <$ runExcept (foldProg (Prog (Imports [imp]) [] (Loc NoPos (List []))))
  where
    imp = ImportDecl
      { importLib       = LibName "Fake"
      , importAlias     = Nothing
      , importExports   = Just []
      , importSelected  = Nothing
      , importMode      = Qualified
      , importDatatypes = dts
      }
