import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { assertIsLevel, assertIsNTuple, assertIsCapability, assertIsAuthority } from '../Asserts';


export function BuiltinMboxClear <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        
        raisembox = mkBase((arg) => {
            assertIsLevel(arg);
            return this.runtime.$t.raiseMboxClearance(arg)
        })

        lowermbox = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsCapability(arg.val[0]);
            assertIsAuthority(arg.val[1]);
            return this.runtime.$t.lowerMboxClearance(arg.val[0], arg.val[1])
        })
         
    }
}


