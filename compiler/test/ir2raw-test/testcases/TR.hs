module TR where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo


mkP :: IRTerminator -> IRProgram
mkP tr = IRProgram (Core.Atoms []) [FunDef (HFN "main") (VN "arg") [] body]
  where body = BB [] tr

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
       (BB [Assign (VN "b1") (Base "v1") ] (LibExport (mkV "b1")))
       (BB [Assign (VN "b2") (Base "v2") ] (LibExport (mkV "b2")))
  ),
  ( "Call"
  , Call (VN "x")
       (BB [Assign (VN "b1") (Base "v1") ] (LibExport (mkV "b1")))
       (BB [Assign (VN "b2") (Base "v2") ] (LibExport (mkV "b2")))
  ),
  ( "AssertElseError"
  , AssertElseError (mkV "x") (BB [Assign (VN "b") (Base "v")] (LibExport (mkV "b"))) (mkV "verr") NoPos
  ),
  ( "Error"
  , Error (mkV "verr") NoPos
  )
  ]
