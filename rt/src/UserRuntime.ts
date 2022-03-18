import { UserRuntimeZero } from './builtins/UserRuntimeZero'
import { BuiltinStdIo } from './builtins/stdio'
import { BuiltinMath } from './builtins/math'
import { BuiltinDebugUtils } from './builtins/debugutils'
import { BuiltinPini } from './builtins/pini'
import { BuiltinMonitors } from './builtins/monitor'
import { BuiltinSleep } from './builtins/sleep'
import { BuiltinLevOps } from './builtins/levelops'
import { BuiltinMboxClear } from './builtins/mboxclear'
import { BuiltinMkUuid } from './builtins/mkuuid'
import { BuiltinPersist } from './builtins/persist'
import { BuiltinNodeUtils } from './builtins/nodeutil'
import { BuiltinSelf } from './builtins/self'
import { BuiltinExit } from './builtins/exit'
import { BuiltinAdv } from './builtins/adv'
import { BuiltinGetTime } from './builtins/getTime'
import { BuiltinStringToInt } from './builtins/stringToInt'
import { BuiltinToString } from './builtins/toString'
import { BuiltinSend } from './builtins/send'
import { BuiltinSpawn } from './builtins/spawn'
import { BuiltinReceive } from './builtins/receive'
import { BuiltinAttenuate } from './builtins/attenuate'
import { BuiltinRegistry } from './builtins/whereis'
import { BuiltinDeclassify } from './builtins/declassify'
import { BuiltinRaiseTrust } from './builtins/raiseTrust'
import { BuiltinSandbox } from './builtins/sandbox'
import { BuiltinLocalArrays } from './builtins/localarrays'
import { RuntimeAssert } from './builtins/runtimeassert'
import { BuiltinService } from './builtins/service'
import { BuiltinString } from './builtins/string'

let BuiltSpawnSendReceive = x => BuiltinSpawn(BuiltinSend(BuiltinReceive(x)))

export const UserRuntime =
    BuiltinString (
    BuiltinService (
    RuntimeAssert(
    BuiltinLocalArrays(
    BuiltinSandbox(
    BuiltinRaiseTrust(
    BuiltinDeclassify (
    BuiltinRegistry(
    BuiltinAttenuate(
    BuiltSpawnSendReceive(
    BuiltinStringToInt(
    BuiltinToString(
    BuiltinGetTime(
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
    BuiltinDebugUtils(
    BuiltinMath(
    BuiltinStdIo(UserRuntimeZero)))))))))))))))))))))))))))
