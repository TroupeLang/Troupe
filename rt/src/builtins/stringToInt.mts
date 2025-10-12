'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsNumber, assertIsString } from '../Asserts.mjs';


export function BuiltinStringToInt<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        stringToInt = mkBase((arg) => {
            assertIsString(arg);
            let r = this.runtime.$t.mkValWithLev(parseFloat(arg.val), arg.lev);
            return this.runtime.ret(r);
        }, "stringToInt")

        intToString = mkBase((arg) => {
            assertIsNumber (arg);
            let r = this.runtime.$t.mkValWithLev( arg.val.toString(), arg.lev );
            return this.runtime.ret (r);
        })
    }
}
