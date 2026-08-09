-- | The whitelist of ambient base functions: the names an unresolved variable
-- may fall through to ('Core' resolves them to @BaseName@, the runtime provides
-- them). One list, three consumers:
--
--   * "IR" — the well-formedness check rejects a @Base@ reference outside the
--     list, including in deserialized mobile code;
--   * "Core" — the renamer's missing-require error fires only for names that
--     are native-manifest names and /not/ base functions, so a manifest can
--     never shadow a base function;
--   * "ProcessImports" — a native-module manifest declaring a base-function
--     name is rejected when the manifest is read.
module BaseFunctions
  ( baseFunctions
  , isBaseFunction
  ) where

import qualified Data.Set as Set
import           Basics (VarName)

isBaseFunction :: VarName -> Bool
isBaseFunction = (`Set.member` baseFunctions)

baseFunctions :: Set.Set VarName
baseFunctions = Set.fromList
                     [
                       "$$authorityarg"
                     , "adv"
                     , "ladv"
                     , "attenuate"
                     , "_blockThread"
                     , "blockdecl"
                     , "blockdeclto"
                     , "blockdown"
                     , "blockdownto"
                     , "blockendorse"
                     , "blockendorseto"
                     , "bigAdd"
                     , "bigSub"
                     , "bigMul"
                     , "bigDiv"
                     , "bigMod"
                     , "bigNeg"
                     , "bigCmp"
                     , "bigFromInt"
                     , "bigFromLiteral"
                     , "bigFromString"
                     , "bigToInt"
                     , "bigToString"
                     , "base64Decode"
                     , "base64Encode"
                     , "cert"
                     , "charCodeAtWithDefault"
                     , "charFromCode"
                     , "coalesce"
                     , "consume"
                     , "consumeWithAuthority"
                     , "_debug"
                     , "debugMbox"
                     , "debugpc"
                     , "debugValue"
                     , "declassify"
                     , "declassifyType"
                     , "disableRangedReceive"
                     , "downgrade"
                     , "downgradeType"
                     , "enableRangedReceive"
                     , "endorse"
                     -- SimpleFileIO whole-file primitives (ROOT-authority; see builtins/simplefileio.mts)
                     , "appendFile"
                     , "fileExists"
                     , "fileStat"
                     , "makeDir"
                     , "readDir"
                     , "readFile"
                     , "readFileBytes"
                     , "removeFile"
                     , "writeFile"
                     , "writeFileBytes"
                     , "endorseType"
                     , "exit"
                     , "floor"
                     , "flowsTo"
                     , "freadln"
                     , "freadlnAtLevel"
                     , "fwrite"
                     -- The ambient names: what `print` and its neighbours
                     -- resolve to, in a program, a module or a library alike
                     -- (builtins/stdio.mts).
                     , "fwriteln"
                     , "fwritelnWithLabels"
                     , "inputLine"
                     , "print"
                     , "printString"
                     , "printWithLabels"
                     , "getTime"
                     , "getCliArgs"
                     , "getType"
                     , "gunzip"
                     , "gzip"
                     , "getNanoTime"
                     , "_getSystemProcess"
                     , "guard"
                     , "intToString"
                     , "listToTuple"
                     , "levelOf"
                     , "mkuuid"
                     , "mkSecret"
                     , "monitorlocal"
                     , "newlabel"
                     , "node"
                     , "_pc"
                     , "_bl"
                  -- , "pcpop"
                     , "peek"
                     , "pinipush"
                     , "pinipushto"
                     , "pinipop"
                    --  , "pcpush"
                     , "raiseTrust"
                     , "random"
                     , "receive"
                     , "recordExtend"
                     , "recordToList"
                     , "register"
                     , "_resetScheduler"
                     , "rcv"
                     , "rcvp"
                     , "sandbox"
                     , "save"
                     , "send"
                     , "self"
                     , "_servicetest"
                     , "_setProcessDebuggingName"
                     , "_setFailureRate"
                     -- The channel level, moved under full authority
                     -- (builtins/stdio.mts).
                     , "setStdioLevel"
                     , "sleep"
                     , "spawn"
                     , "sqrt"
                     , "substring"
                     , "stdin"
                     , "stdout"
                     , "stderr"
                     , "strIndexOf"
                     , "stringToInt"
                     , "strlen"
                     , "restore"
                     , "toStringL"
                     , "toString"
                     -- Signal disposition (see builtins/signals.mts)
                     , "trapSigterm"
                     -- Terminal primitives (see builtins/tty.mts)
                     , "ttyIsTTY"
                     , "ttyLevel"
                     , "ttyRawMode"
                     , "ttySize"
                     , "ttySubscribe"
                     , "ttyUnsubscribe"
                     , "untrapSigterm"
                     , "whereis"
                     ]
