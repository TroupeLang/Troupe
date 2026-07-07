-- | Test helpers shared with the ir2raw-test corpus modules (Expr/Inst/TR/Tree
-- import @Util@). Kept identical to test/ir2raw-test/Util.hs so those modules
-- can be reused verbatim from this suite's source tree.
module Util where

import qualified IR
import RetCPS (VarName(..))
import TroupePositionInfo (Located(Loc), PosInf(..))

-- Create a Located VarAccess with NoPos for test purposes
mkV :: String -> IR.LVarAccess
mkV s = Loc NoPos (IR.VarLocal (VN s))

-- Create a Located VarName with NoPos for FunDef arguments
mkVN :: String -> IR.LVarName
mkVN s = Loc NoPos (VN s)

-- Wrap an instruction with NoPos
mkLInst :: IR.IRInst -> IR.LIRInst
mkLInst inst = Loc NoPos inst

-- Wrap a terminator with NoPos
mkLTerm :: IR.IRTerminator -> IR.LIRTerminator
mkLTerm term = Loc NoPos term
