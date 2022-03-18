import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { assertNormalState, assertIsProcessId, assertIsString } from '../Asserts';
import { __unit } from '../UnitVal';

export function BuiltinMonitors <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    
    return class extends Base {
        monitorlocal = mkBase((arg) => {
            assertNormalState("monitor");
            this.runtime.$t.raiseCurrentThreadPC(arg.lev);
            assertIsProcessId(arg);

            let tid = arg.val;

            // 1. find the thread corresponding to that tid 

            let t = this.runtime.__sched.__alive[tid.toString()];
            // 2. update the monitor state of that thread

            let r = this.runtime.rt_mkuuid();
            if (t) {
                t.addMonitor(this.runtime.__sched.__currentThread.tid, r);
            }

            return this.runtime.ret(r);
        })


        demonitorlocal = mkBase((arg) => {
            assertIsString(arg);
            // mutates state; so we should be careful...
            return this.runtime.ret(__unit);
        })
    }
}