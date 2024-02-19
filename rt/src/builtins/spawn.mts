import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import {lub} from '../options.mjs'
import { assertNormalState, assertIsFunction, assertIsNode } from '../Asserts.mjs'
import { __nodeManager } from '../NodeManager.mjs';
import { __unit } from '../UnitVal.mjs';


export function BuiltinSpawn<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        spawn = mkBase((larg) => {
            assertNormalState("spawn")
            // debug ("* rt rt_spawn *", larg.val, larg.lev);
            // console.log ("SPAWN ARGS", larg)
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            let arg = larg.val;
            let __sched = this.runtime.__sched
            

            let spawnLocal = (arg) => {
                // debug ("scheduled rt_spawn ", arg.fun);

                let newPid = __sched.scheduleNewThreadAtLevel(
                    arg,
                    __unit, // [arg.env, __unit],
                    // arg.namespace,
                    this.runtime.$t.pc,
                    this.runtime.$t.bl)
                return this.runtime.$t.returnImmediateLValue(newPid) ;
            }


            if (Array.isArray(arg)) {
                if (__nodeManager.isLocalNode(arg[0].val)) { // check if we are at the same node or note
                    // debug ("SAME NODE")
                    this.runtime.$t.raiseCurrentThreadPC(lub(arg[0].lev, arg[1].lev));
                    assertIsFunction(arg[1]);
                    return spawnLocal(arg[1].val)
                } else {
                    assertIsNode(arg[0]);
                    assertIsFunction(arg[1]);
                    (async () => this.runtime.spawnAtNode(arg[0], arg[1]))()

                }
            } else {
                assertIsFunction(larg);
                return spawnLocal(arg)
            }
        }, "spawn");
    }
}