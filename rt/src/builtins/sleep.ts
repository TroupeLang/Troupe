import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { assertIsNumber } from '../Asserts'
import { __unit } from '../UnitVal'
import { SleepTimeout } from '../Thread'
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