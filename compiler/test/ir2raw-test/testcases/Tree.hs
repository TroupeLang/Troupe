
module Tree where

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import Control.Arrow
import TroupePositionInfo
import qualified Basics


mkP :: IRBBTree -> IRProgram
mkP tree = IRProgram [Loc NoPos (FunDef (HFN "main") (mkVN "arg") [] tree)]

tcs :: [(String, IRProgram)]
tcs = map (second mkP)
  [ ( "TreeEmpty"
    , BB [] (mkLTerm (Ret (mkV "r")))
    )
  ,
    ( "TreeAssign"
    , BB [mkLInst (Assign (VN "r") (Tuple [] False))] (mkLTerm (Ret (mkV "r")))
    )
  ]
