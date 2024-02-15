
module Tree where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo
import qualified Basics


mkP :: IRBBTree -> IRProgram
mkP tree = IRProgram (Core.Atoms []) [FunDef (HFN "main") (VN "arg") [] tree]

tcs :: [(String, IRProgram)]
tcs = map (second mkP)
  [ ( "TreeEmpty"
    , BB [] (Ret (mkV "r"))
    )
  ,
    ( "TreeAssign"
    , BB [Assign (VN "r") (Tuple [])] (Ret (mkV "r"))
    )
  ]
