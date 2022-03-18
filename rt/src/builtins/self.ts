import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';
import options from '../options'
const levels = options;

/**
 * Returns a string corresponding to the node identify
 * from a process
 */

export function BuiltinSelf<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        self = mkBase((arg) => {
            return this.runtime.ret(this.runtime.__sched.__currentThread.tid);
        }, "self");
    }
}