import { UserRuntimeZero } from './builtins/UserRuntimeZero.mjs'
import { BuiltinStdIo } from './builtins/stdio.mjs'
import { BuiltinSignals } from './builtins/signals.mjs'
import { BuiltinMath } from './builtins/math.mjs'
import { BuiltinBigInt } from './builtins/bigint.mjs'
import { BuiltinDebugUtils } from './builtins/debugutils.mjs'
import { BuiltinDebugValue } from './builtins/debugValue.mjs'
import { BuiltinPini } from './builtins/pini.mjs'
import { BuiltinMonitors } from './builtins/monitor.mjs'
import { BuiltinSleep } from './builtins/sleep.mjs'
import { BuiltinLevOps } from './builtins/levelops.mjs'
import { BuiltinMboxClear } from './builtins/mboxclear.mjs'
import { BuiltinMkUuid } from './builtins/mkuuid.mjs'
import { BuiltinPersist } from './builtins/persist.mjs'
import { BuiltinNodeUtils } from './builtins/nodeutil.mjs'
import { BuiltinSelf } from './builtins/self.mjs'
import { BuiltinExit } from './builtins/exit.mjs'
import { BuiltinAdv } from './builtins/adv.mjs'
import { BuiltinGetTime } from './builtins/getTime.mjs'
import { BuiltinCliArgs } from './builtins/cliargs.mjs'
import { BuiltinStringToInt } from './builtins/stringToInt.mjs'
import { BuiltinToString } from './builtins/toString.mjs'
import { BuiltinSend } from './builtins/send.mjs'
import { BuiltinSpawn } from './builtins/spawn.mjs'
import { BuiltinReceive } from './builtins/receive.mjs'
import { BuiltinThread } from './builtins/thread.mjs'
import { BuiltinAttenuate } from './builtins/attenuate.mjs'
import { BuiltinCoalesce } from './builtins/coalesce.mjs'
import { BuiltinRegistry } from './builtins/whereis.mjs'
import { BuiltinDeclassify } from './builtins/declassify.mjs'
import { BuiltinRaiseTrust } from './builtins/raiseTrust.mjs'
import { BuiltinSandbox } from './builtins/sandbox.mjs'
import { RuntimeAssert } from './builtins/runtimeassert.mjs'
import { BuiltinService } from './builtins/service.mjs'
import { BuiltinString } from './builtins/string.mjs'
import { BuiltinRecordReflection } from './builtins/recordReflection.mjs'
import { BuiltinRecordToList } from './builtins/recordToList.mjs'
import { BuiltinTypeInformation } from './builtins/types.mjs'
import { BuiltinListToTuple } from './builtins/listToTuple.mjs'
import { BuiltinCodec } from './builtins/codec.mjs'

let BuiltSpawnSendReceive = x => BuiltinSpawn(BuiltinSend(BuiltinReceive(x)))

export const UserRuntime =
    BuiltinCodec (
    BuiltinListToTuple (
    BuiltinString (
    BuiltinService (
    RuntimeAssert(
    BuiltinSandbox(
    BuiltinRaiseTrust(
    BuiltinDeclassify (
    BuiltinRegistry(
    BuiltinCoalesce(
    BuiltinAttenuate(
    BuiltSpawnSendReceive(
    BuiltinThread(
    BuiltinStringToInt(
    BuiltinToString(
    BuiltinGetTime(
    BuiltinCliArgs(
    BuiltinAdv(
    BuiltinExit(
    BuiltinSelf(
    BuiltinNodeUtils(
    BuiltinPersist(
    BuiltinMkUuid(
    BuiltinMboxClear(
    BuiltinLevOps(
    BuiltinSleep(
    BuiltinMonitors(
    BuiltinPini(
    BuiltinDebugValue(
    BuiltinDebugUtils(
    BuiltinMath(
    BuiltinBigInt(
    BuiltinRecordToList(
    BuiltinRecordReflection(
    BuiltinTypeInformation(
    BuiltinSignals(
    BuiltinStdIo(UserRuntimeZero)
    ))))))))))))))))))))))))))))))))))))
