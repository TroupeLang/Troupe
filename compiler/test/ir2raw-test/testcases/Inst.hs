module Inst where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo
import qualified Basics


mkP :: IRInst -> IRProgram
mkP inst = IRProgram [Loc NoPos (FunDef (HFN "main") (mkVN "arg") [] body)]
  where body = BB [mkLInst inst] (mkLTerm (LibExport (mkV "r")))

tcs :: [(String, IRProgram)]
tcs = map (second mkP)
  [ ( "AssignSimple"
    , Assign (VN "r") (Const $ Core.LNumeric (Core.NumInt 123))
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
