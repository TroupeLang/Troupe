'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';
import { assertIsUnit, assertNormalState } from '../Asserts';


export function BuiltinGetTime<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        getTime = mkBase((arg) => {
            assertNormalState("getTime")
            assertIsUnit(arg)
            let d = new Date()
            let t = d.getTime()
            let v = new LVal(t, this.runtime.$t.pc);
            return this.runtime.ret(v)
        })

        getNanoTime = mkBase((arg) => {
            assertIsUnit(arg)
            let t = process.hrtime.bigint()
            let v = new LVal(t, this.runtime.$t.pc);
            return this.runtime.ret(v)
        })

    }
}
