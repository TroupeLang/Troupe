import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsLevel, assertIsNTuple, assertIsCapability, assertIsAuthority, assertNormalState } from '../Asserts.mjs'


export function BuiltinMboxClear <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        // Ranged receive: open a clearance region ⟨lo, hi⟩, certified at the open by the
        // shown authority. The null authority is legal (it yields ok_to_dg = false / a
        // confined region rather than a type error).
        enableRangedReceive = mkBase((arg) => {
            assertNormalState("enableRangedReceive");
            assertIsNTuple(arg, 3);
            assertIsLevel(arg.val[0]);
            assertIsLevel(arg.val[1]);
            assertIsAuthority(arg.val[2]);
            return this.runtime.$t.enableRangedReceive(arg.val[0], arg.val[1], arg.val[2])
        })

        // Ranged receive: close a region. Authority-free — the capability is the certificate.
        disableRangedReceive = mkBase((arg) => {
            assertNormalState("disableRangedReceive");
            assertIsCapability(arg);
            return this.runtime.$t.disableRangedReceive(arg)
        })

    }
}


