import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as levels from '../options.mjs'
import { assertIsNTuple, assertNormalState } from '../Asserts.mjs';
import { __unit } from '../UnitVal.mjs';

const {lub, flowsTo} = levels

export function BuiltinAdv <TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        mkSecret = mkBase((x) => {
            // debug ("making secret " + x.val)
            this.runtime.$t.invalidateSparseBit()
            return this.runtime.ret(new LVal(x.val, levels.TOP))
        })

        adv = mkBase((x) => {
            assertNormalState("baseDisclose");
            // assert that
            // pc ⊔ x.lev ⊑ LOW
            let __sched = this.runtime.__sched

            if (!flowsTo(lub(this.runtime.$t.bl, x.lev), levels.BOT)) {
                this.runtime.$t.
                threadError("Illegal flow in adv function:\n" +
                    ` |    pc: ${this.runtime.$t.pc.stringRep()}\n` +
                    ` | block: ${this.runtime.$t.bl.stringRep()}\n` +
                    ` | value: ${x.stringRep()}`)
            }
            return this.runtime.ret(__unit);
        })

        ladv = mkBase((x) => {
            assertNormalState("ladv");
            assertIsNTuple(x, 2)
            let l_adv = x.val[0] 
            let value = x.val[1]
            // assert that
            // pc ⊔ x.lev ⊑ LOW

            if (!flowsTo(lub(this.runtime.$t.bl, value.lev, l_adv.lev), l_adv.val)) {
                this.runtime.$t.
                  threadError("Illegal flow in adv function:\n" +
                    ` |    pc: ${this.runtime.$t.pc.stringRep()}\n` +
                    ` | block: ${this.runtime.$t.bl.stringRep()}\n` +
                    ` | l_adv: ${l_adv.stringRep()} \n` +
                    ` | value: ${value.stringRep()}`)
            }
            return this.runtime.ret(__unit);
        })
         
    }
}