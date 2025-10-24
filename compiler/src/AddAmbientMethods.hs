-- 2020-05-17, AA

-- HACK
--
-- This module add a number of standard ambient methods such as `print` to the beginning of the
-- file. This provides some backward compatibility with prior test cases as well as minimizes some
-- clutter.
--
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

import Basics
import Direct
import TroupePositionInfo

printStringDecl :: FunDecl
printStringDecl = FunDecl "printString"
    [Lambda [VarPattern "x"] $
        Let [ ValDecl (VarPattern "fd") (App (Var "stdout") [Var "authority"]) NoPos
            , ValDecl (VarPattern "x'") (Bin Concat (Var "x") (Lit $ LString "\\n")) NoPos
            ]
            (App (Var "fwrite") [Tuple [Var "fd", Var "x'"]])
    ] NoPos

printDecl :: FunDecl
printDecl = FunDecl "print"
    [Lambda [ VarPattern "x" ] $
      (App (Var "printString") [App (Var "toString") [Var "x"]])
    ] NoPos

printLDecl :: FunDecl
printLDecl = FunDecl "printL"
    [Lambda [ VarPattern "x" ] $
      (App (Var "printString") [App (Var "toStringL") [Var "x"]])
    ] NoPos

inputLineDecl :: FunDecl
inputLineDecl = FunDecl "inputLine"
    [Lambda [ VarPattern "_" ] $
        Let [ValDecl (VarPattern "fd") (App (Var "stdin") [Var "authority"]) NoPos]
            (App (Var "freadln") [App (Var "stdin") [Var "authority"]])
    ] NoPos

addAmbientMethods :: Prog -> Prog
addAmbientMethods (Prog imports atoms t) =
    let t' = Let [FunDecs [printStringDecl,printDecl,printLDecl,inputLineDecl]] t
    in Prog imports atoms t'
