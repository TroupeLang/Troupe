import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsUnit } from '../Asserts.mjs'


export function BuiltinMkUuid <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        mkuuid = mkBase((arg) => {
            assertIsUnit(arg);
            return this.runtime.ret(this.runtime.rt_mkuuid());
        });
    }
}
