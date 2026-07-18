-- 2020-05-17, AA

-- HACK
--
-- This module add a number of standard ambient methods such as `print` to the beginning of the
-- file. This provides some backward compatibility with prior test cases as well as minimizes some
-- clutter
-- If these methods are unused they are eliminated by the optimization passes in the further passes.

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
    lterm $ Seq [lterm $ App (lterm $ Var "fwrite") [lterm $ Tuple [lterm $ Var "fd", lterm $ Var "x"]]
                , lterm $ App (lterm $ Var "fwrite") [lterm $ Tuple [lterm $ Var "fd", lterm $ Lit $ LString "\\n"]]
        ]
  ]

fwritelnWithLabelsDecl :: LFunDecl
fwritelnWithLabelsDecl = lp $ FunDecl "fwritelnWithLabels"
  [Lambda [lpat $ TuplePattern [lpat $ VarPattern "fd", lpat $ VarPattern "x"] ] $
    lterm $ App (lterm $ Var "fwriteln") [lterm $ Tuple [lterm $ Var "fd"
                                                        , lterm $ App (lterm $ Var "toStringL") [lterm $ Var "x"]]]
  ]

printStringDecl :: LFunDecl
printStringDecl = lp $ FunDecl "printString"
    [Lambda [lpat $ VarPattern "x" ] $
      lterm $ Let [ ValDecl (lpat $ VarPattern "fd") (lterm $ App (lterm $ Var "stdout")
                                                              [lterm $ Var "authority"])] $
          (lterm $ App (lterm $ Var "fwriteln") [lterm $ Tuple [lterm $ Var "fd", lterm $ Var "x"]])
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
addAmbientMethods (Prog imports atoms groups t) =
    let t' = lterm $ Let [FunDecs [ fwritelnDecl
                          , fwritelnWithLabelsDecl
                          , printStringDecl
                          , printDecl
                          , printWithLabelsDecl
                          , inputLineDecl]
                 ] t
    in Prog imports atoms groups t'
