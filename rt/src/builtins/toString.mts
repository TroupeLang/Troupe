'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';


export function BuiltinToString<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        toString = mkBase((arg) => {
            let taintRef = { lev: this.runtime.$t.pc };
            let s = this.runtime.$t.mkCopy(arg).stringRep
                (true,  // omit labels
                    taintRef  // accumulate taint into this reference
                )

            let r = this.runtime.$t.mkValWithLev(s, taintRef.lev);
            return this.runtime.ret(r);
        }, "toString")


        toStringL = mkBase((arg) => {
            let v = this.runtime.$t.mkCopy(arg);
            let taintRef = { lev: this.runtime.$t.pc };

            let s = v.stringRep(false,  // do not omit labels 
                taintRef  // accumulate taint into this reference
            )

            let r = this.runtime.$t.mkValWithLev(s, taintRef.lev);
            return this.runtime.ret(r);
        }, "toStringLabeled")
    }
}