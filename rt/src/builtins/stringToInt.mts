'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';


export function BuiltinStringToInt<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        stringToInt = mkBase((arg) => {
            let r = this.runtime.$t.mkValWithLev(parseInt(arg.val), arg.lev);
            return this.runtime.ret(r);
        }, "stringToInt")
    }
}
