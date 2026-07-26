module TR where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo


mkP :: IRTerminator -> IRProgram
mkP tr = IRProgram [Loc NoPos (FunDef (HFN "main") (mkVN "arg") [] body)]
  where body = BB [] (mkLTerm tr)

tcs :: [(String, IRProgram)]
tcs = map (second mkP)
  [
  ( "TailCall"
  , TailCall (mkV "x") (mkV "y")
  ),
  ( "Ret"
  , Ret (mkV "x")
  ),
  ( "LibExport"
  , LibExport (mkV "x")
  ),
  -- NOTE: We use libexport as terminator because it generates least extra code
  ( "If"
  , If (mkV "x")
       (BB [mkLInst (Assign (VN "b1") (Base "v1"))] (mkLTerm (LibExport (mkV "b1"))))
       (BB [mkLInst (Assign (VN "b2") (Base "v2"))] (mkLTerm (LibExport (mkV "b2"))))
  ),
  ( "StackExpand"
  , StackExpand (VN "x")
       (BB [mkLInst (Assign (VN "b1") (Base "v1"))] (mkLTerm (LibExport (mkV "b1"))))
       (BB [mkLInst (Assign (VN "b2") (Base "v2"))] (mkLTerm (LibExport (mkV "b2"))))
  ),
  ( "AssertElseError"
  , AssertElseError (mkV "x") (BB [mkLInst (Assign (VN "b") (Base "v"))] (mkLTerm (LibExport (mkV "b")))) (mkV "verr")
  ),
  ( "Error"
  , Error (mkV "verr")
  )
  ]
