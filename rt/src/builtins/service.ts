import {UserRuntimeZero, Constructor, mkBase, mkService} from './UserRuntimeZero'
import { __unit } from '../UnitVal'
export function BuiltinService <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        _servicetest = mkService(() => this.runtime.$service.servicetest(),"servicetest")
    }
}