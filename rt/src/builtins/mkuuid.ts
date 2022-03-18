import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { assertIsUnit } from '../Asserts';


export function BuiltinMkUuid <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        mkuuid = mkBase((arg) => {
            assertIsUnit(arg);
            return this.runtime.ret(this.runtime.rt_mkuuid());
        });
    }
}
