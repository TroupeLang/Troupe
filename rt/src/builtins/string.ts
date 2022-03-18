import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero'
import { LVal } from '../Lval';
import options from '../options'
import { assertIsUnit, assertIsString, assertIsNTuple, assertIsNumber } from '../Asserts';
import { lub, lubs } from '../levels/tagsets';
const levels = options;


export function BuiltinString <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        substring = mkBase (arg => {
            assertIsNTuple(arg, 3)
            assertIsString(arg.val[0])
            assertIsNumber(arg.val[1])
            assertIsNumber(arg.val[2])
            let s = arg.val[0].val
            let i = arg.val[1].val 
            let j = arg.val[2].val 
            let s2 = s.substring (i,j) 
            return this.runtime.ret (new LVal(s2, lub ( arg.lev
                                                      , arg.val[0].lev
                                                      , arg.val[1].lev
                                                      , arg.val[2].lev
                                                      )))
        })
    }
 
}