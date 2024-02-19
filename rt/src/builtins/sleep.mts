import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsNumber } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs'
import { SleepTimeout } from '../Thread.mjs'
export function BuiltinSleep <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        sleep = mkBase((arg) => {
            let $r = this.runtime
            assertIsNumber (arg);
            let delay = arg.val; 
            let theThread = $r.$t;
            theThread.raiseBlockingThreadLev(arg.lev);
            theThread.sleepTimeout = new SleepTimeout (  delay, theThread )
            return null;
        }, "sleep")
    }
}