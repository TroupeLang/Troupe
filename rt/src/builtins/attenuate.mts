import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as levels from '../options.mjs'
import { Authority } from '../Authority.mjs';
import { assertIsNTuple, assertIsAuthority, assertIsLevel } from '../Asserts.mjs'
const {lubs, flowsTo} = levels 


export function BuiltinAttenuate<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        attenuate = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            let argv = arg.val;
            let authFrom = argv[0];
            assertIsAuthority(authFrom);
            let levTo = argv[1];
            assertIsLevel(levTo);

            let ok_to_attenuate = flowsTo(levTo.val, authFrom.val.authorityLevel);

            // todo: 2018-10-18: AA; are we missing anything?
            let l_meta = lubs([this.runtime.$t.pc, arg.lev, authFrom.lev, levTo.lev])
            let l_auth = ok_to_attenuate ? levTo.val : levels.BOT;
            let r = new LVal(new Authority(l_auth), l_meta)

            return this.runtime.ret(r)
        }, "attenuate")

    }
}