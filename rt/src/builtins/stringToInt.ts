'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';


export function BuiltinStringToInt<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        stringToInt = mkBase((arg) => {
            let r = this.runtime.$t.mkValWithLev(parseInt(arg.val), arg.lev);
            return this.runtime.ret(r);
        }, "stringToInt")
    }
}
