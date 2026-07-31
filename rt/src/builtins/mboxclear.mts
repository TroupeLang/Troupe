import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsLevel, assertIsNTuple, assertIsCapability, assertIsAuthority, assertNormalState } from '../Asserts.mjs'


export function BuiltinMboxClear <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        raisembox = mkBase((arg) => {
            assertNormalState("raisembox");
            assertIsLevel(arg);
            return this.runtime.$t.raiseMboxClearance(arg)
        })

        lowermbox = mkBase((arg) => {
            assertNormalState("lowermbox");
            assertIsNTuple(arg, 2);
            assertIsCapability(arg.val[0]);
            assertIsAuthority(arg.val[1]);
            return this.runtime.$t.lowerMboxClearance(arg.val[0], arg.val[1])
        })
         
    }
}


