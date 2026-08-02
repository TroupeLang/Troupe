-- 2020-05-17, AA

-- The ambient wrappers of the capability stdio model: `print` and its neighbours, defined at the
-- outermost scope of a program so that printing needs no descriptor at the call site.
--
-- Each wrapper acquires its descriptor with the program's own `authority` — the binding
-- "CaseElimination" introduces for main's argument — and the runtime checks that authority
-- against the channel level. That is what ties them to the capability model: "Pipeline" injects
-- them only for it. A program compiled for the IFC model resolves the same six names to the
-- runtime's builtins (@rt/src/builtins/stdio.mts@), which acquire nothing and carry the channel
-- check on the write instead.
--
-- All six are injected whether or not the program names them, and no later pass drops the unused
-- ones: they reach the emitted JavaScript of every capability-model program.

-- TODO
--
-- Move this into a '.trp' file of the form
--
-- ```
--     let fun print x = fwrite (stdout authority, (toString x) ^"\n")
--         ...
--     in () end
-- ```
--
-- Which, similar to below, after parsing has the `dummy` value replaced by the actual program. This
-- preamble can then be specified at compile-time.

module AddAmbientMethods(addAmbientMethods) where

import Direct

import TroupePositionInfo (Located(..), PosInf(..))

-- Helper to create Located values at NoPos
lp :: a -> Located a
lp = Loc NoPos

-- Helper to create Located patterns at NoPos
lpat :: DeclPattern -> LDeclPattern
lpat = lp

-- Helper to create Located terms at NoPos
lterm :: Term -> LTerm
lterm = lp

fwritelnDecl :: LFunDecl
fwritelnDecl = lp $ FunDecl "fwriteln"
  [Lambda [lpat $ TuplePattern [lpat $ VarPattern "fd", lpat $ VarPattern "x"] ] $
    lterm $ Seq [lterm $ App (lterm $ Var "fwrite") [lterm $ Tuple [lterm $ Var "fd", lterm $ Var "x"] False]
                , lterm $ App (lterm $ Var "fwrite") [lterm $ Tuple [lterm $ Var "fd", lterm $ Lit $ LString "\\n"] False]
        ]
  ]

fwritelnWithLabelsDecl :: LFunDecl
fwritelnWithLabelsDecl = lp $ FunDecl "fwritelnWithLabels"
  [Lambda [lpat $ TuplePattern [lpat $ VarPattern "fd", lpat $ VarPattern "x"] ] $
    lterm $ App (lterm $ Var "fwriteln") [lterm $ Tuple [lterm $ Var "fd"
                                                        , lterm $ App (lterm $ Var "toStringL") [lterm $ Var "x"]] False]
  ]

printStringDecl :: LFunDecl
printStringDecl = lp $ FunDecl "printString"
    [Lambda [lpat $ VarPattern "x" ] $
      lterm $ Let [ ValDecl (lpat $ VarPattern "fd") (lterm $ App (lterm $ Var "stdout")
                                                              [lterm $ Var "authority"])] $
          (lterm $ App (lterm $ Var "fwriteln") [lterm $ Tuple [lterm $ Var "fd", lterm $ Var "x"] False])
    ]

printDecl :: LFunDecl
printDecl = lp $ FunDecl "print"
    [Lambda [lpat $ VarPattern "x"] $
      (lterm $ App (lterm $ Var "printString") [lterm $ App (lterm $ Var "toString") [lterm $ Var "x"]])
    ]

printWithLabelsDecl :: LFunDecl
printWithLabelsDecl = lp $ FunDecl "printWithLabels"
    [Lambda [lpat $ VarPattern "x"] $
      (lterm $ App (lterm $ Var "printString") [lterm $ App (lterm $ Var "toStringL") [lterm $ Var "x"]])
    ]

inputLineDecl :: LFunDecl
inputLineDecl = lp $ FunDecl "inputLine"
    [Lambda [lpat $ VarPattern "_"] $
        lterm $ Let [ ValDecl (lpat $ VarPattern "fd") (lterm $ App (lterm $ Var "stdin") [lterm $ Var "authority"])]
                    (lterm $ App (lterm $ Var "freadln") [lterm $ App (lterm $ Var "stdin") [lterm $ Var "authority"]])
    ]

addAmbientMethods :: Prog -> Prog
addAmbientMethods (Prog imports groups t) =
    let t' = lterm $ Let [FunDecs [ fwritelnDecl
                          , fwritelnWithLabelsDecl
                          , printStringDecl
                          , printDecl
                          , printWithLabelsDecl
                          , inputLineDecl]
                 ] t
    in Prog imports groups t'
