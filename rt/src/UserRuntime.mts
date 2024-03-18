import { UserRuntimeZero } from './builtins/UserRuntimeZero.mjs'
import { BuiltinStdIo } from './builtins/stdio.mjs'
import { BuiltinMath } from './builtins/math.mjs'
import { BuiltinDebugUtils } from './builtins/debugutils.mjs'
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
import { BuiltinStringToInt } from './builtins/stringToInt.mjs'
import { BuiltinToString } from './builtins/toString.mjs'
import { BuiltinSend } from './builtins/send.mjs'
import { BuiltinSpawn } from './builtins/spawn.mjs'
import { BuiltinReceive } from './builtins/receive.mjs'
import { BuiltinAttenuate } from './builtins/attenuate.mjs'
import { BuiltinRegistry } from './builtins/whereis.mjs'
import { BuiltinDeclassify } from './builtins/declassify.mjs'
import { BuiltinRaiseTrust } from './builtins/raiseTrust.mjs'
import { BuiltinSandbox } from './builtins/sandbox.mjs'
import { BuiltinLocalArrays } from './builtins/localarrays.mjs'
import { RuntimeAssert } from './builtins/runtimeassert.mjs'
import { BuiltinService } from './builtins/service.mjs'
import { BuiltinString } from './builtins/string.mjs'
import { BuiltinRecordReflection } from './builtins/recordReflection.mjs'
import { BuiltinTypeInformation } from './builtins/types.mjs'

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
    BuiltinRecordReflection(
    BuiltinTypeInformation(
    BuiltinStdIo(UserRuntimeZero)    
    ))))))))))))))))))))))))))))
