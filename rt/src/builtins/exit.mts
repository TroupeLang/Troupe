import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as options from '../options.mjs'
import { assertNormalState, assertIsNTuple, assertIsAuthority, assertIsNumber, assertIsTopAuthority } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';

const levels = options;
const { lub, flowsTo } = levels

export function BuiltinExit <TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        exit = mkBase((arg) => {
            let $r = this.runtime
            assertNormalState("exit");
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0]);
            assertIsNumber(arg.val[1]);
            assertIsTopAuthority(arg.val[0]);
            (async () => {
                await $r.cleanup()
                process.exit(arg.val[1].val);
            }) ()

        }, "exit")

        _resetScheduler = mkBase((arg) => {
            assertNormalState("exit");
            assertIsAuthority(arg);
            assertIsTopAuthority(arg);
            this.runtime.__sched.resetScheduler ()
            return this.runtime.ret(__unit)
            
        })
    }
}