-- 2020-05-17, AA

-- HACK
-- This module add a number of standard 
-- ambient methods such as `print` to the 
-- beginning of the file. This provides some
-- backward compatibility with prior test cases
-- as well as minimizes some clutter

-- If these methods are unused they are 
-- eliminated by the optimization passes in 
-- the further passes.

module AddAmbientMethods(addAmbientMethods) where 

import Basics
import Direct  
import TroupePositionInfo

printDecl :: FunDecl 
printDecl = FunDecl "print"  
    [Lambda [VarPattern "x"] $
        Let [ValDecl (VarPattern "out") (App (Var "getStdout") [Var "authority"]) NoPos]
            (App (Var "fprintln") [Tuple [Var "out", Var "x"]])
    ] NoPos

printWithLabelsDecl :: FunDecl 
printWithLabelsDecl = FunDecl "printWithLabels" 
    [Lambda [VarPattern "x"] $
        Let [ValDecl (VarPattern "out") (App (Var "getStdout") [Var "authority"]) NoPos]
            (App (Var "fprintlnWithLabels") [Tuple [Var "out", Var "x"]])
    ] NoPos


printStringDecl :: FunDecl 
printStringDecl = FunDecl "printString"
    [Lambda [VarPattern "x"] $
        Let [ValDecl (VarPattern "out") (App (Var "getStdout") [Var "authority"]) NoPos]
            (App (Var "fwrite") [Tuple [Var "out", Bin Concat (Var "x") (Lit (LString "\\n"))]])
    ] NoPos



addAmbientMethods :: Prog -> Prog 
addAmbientMethods (Prog imports atoms t) = 
    let t' = Let [FunDecs [printDecl,printWithLabelsDecl,printStringDecl]] t
    in Prog imports atoms t'