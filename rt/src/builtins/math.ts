import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { LVal } from '../Lval';
import options from '../options'
import { assertIsUnit, assertIsNumber } from '../Asserts';
const levels = options;


export function BuiltinMath <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

         random = mkBase((arg) => {
            assertIsUnit(arg);
            return this.runtime.ret(new LVal(Math.random(), levels.BOT, levels.BOT))
        })

        ceil = mkBase((arg) => {
            assertIsNumber(arg);
            return this.runtime.ret(new LVal(Math.ceil(arg.val), arg.lev, arg.tlev));
        })

        round = mkBase((arg) => {
            assertIsNumber(arg);
            return this.runtime.ret(new LVal(Math.round(arg.val), arg.lev, arg.tlev));
        })

        floor = mkBase((arg) => {
            assertIsNumber(arg);
            return this.runtime.ret(new LVal(Math.floor(arg.val), arg.lev, arg.tlev));
        })
    }
}