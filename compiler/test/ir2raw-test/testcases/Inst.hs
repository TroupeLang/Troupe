module Inst where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo
import qualified Basics


mkP :: IRInst -> IRProgram
mkP inst = IRProgram (Core.Atoms []) [FunDef (HFN "main") (VN "arg") [] body]
  where body = BB [inst] (LibExport (mkV "r"))

tcs :: [(String, IRProgram)]
tcs = map (second mkP)
  [ ( "AssignSimple"
    , Assign (VN "r") (Const $ Core.LInt 123 NoPos)
    )
  ,
    ( "AssignOp"
    , Assign (VN "r") (Bin Basics.Plus (mkV "x") (mkV "y"))
    )
  ,
    ( "AssignEq"
    , Assign (VN "r") (Bin Basics.Eq (mkV "x") (mkV "y"))
    )
  ,
    ( "MkFunClosures"
    , MkFunClosures [(VN "x", mkV "r")] [(VN "f", HFN "f123")]
    )
  ]
