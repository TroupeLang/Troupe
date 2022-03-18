import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';
import levels from '../options'
import { Authority } from '../Authority';
import { assertIsNTuple, assertIsAuthority, assertIsLevel } from '../Asserts';
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