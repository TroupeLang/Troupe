module Util where

import qualified IR
import RetCPS (VarName(..))

mkV s = IR.VarLocal (VN s)