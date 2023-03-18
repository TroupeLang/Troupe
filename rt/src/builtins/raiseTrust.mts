import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LCopyVal } from '../Lval.mjs';
import { assertIsNTuple, assertIsAuthority, assertIsLevel, assertNormalState, assertIsString, assertIsTopAuthority } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import * as options from '../options.mjs'
import { __nodeManager } from '../NodeManager.mjs';
import { nodeTrustLevel, _trustMap } from '../TrustManager.mjs';

const levels = options;
const { lub, flowsTo } = levels

export function BuiltinRaiseTrust<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {

        raiseTrust = mkBase((arg) => {
            assertNormalState("raise trust");
            assertIsNTuple(arg, 3)

            let argv = arg.val;
            let data = argv[0];
            assertIsString(data);

            let authFrom = argv[1];
            assertIsAuthority(authFrom);
            assertIsTopAuthority(authFrom); // AA; 2019-03-07: may be a bit pessimistic, but okay for now
            let levTo = argv[2];
            assertIsLevel(levTo);

            let ok_to_raise =
                flowsTo(this.runtime.$t.bl, levels.BOT);
            if (!ok_to_raise) {
                this.runtime.$t.threadError("Cannot raise trust level when the process is tainted\n" +
                    ` | blocking label: ${this.runtime.$t.bl.stringRep()}`)
            }


            //flowsTo (levTo.val, authFrom.val.authorityLevel);
            // AA, 2018-10-20 : beware that no information flow is enforced here
            // let l_meta = lubs ([__sched.pc, arg.lev, authFrom.lev, levTo.lev])
            let l_raise = ok_to_raise ? levTo.val : levels.BOT;
            let nodeId = __nodeManager.getNode(data.val).nodeId;
            if (!nodeId) {
                this.runtime.$t.threadError(`Undefined node identifier ${data.val}`)
            }
            // let nodeId = data.val;
            let currentLevel = nodeTrustLevel(nodeId)
            _trustMap[nodeId] = lub(currentLevel, l_raise);
            return this.runtime.ret(__unit);
        }, "raiseTrust")

    }
}