import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as  options from '../options.mjs'
const levels = options;
const {lub, flowsTo} = levels
import { v4 as uuidv4 } from 'uuid'
import { assertIsNTuple, assertIsLevel, assertIsUnit } from '../Asserts.mjs'


export function BuiltinLevOps <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
         levelOf = mkBase((arg) => {
            let l = arg.lev;
            return this.runtime.ret(new LVal(l, lub(this.runtime.$t.pc, l), levels.BOT))
        })


        flowsTo = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            let x = arg.val[0];
            let y = arg.val[1];

            assertIsLevel(x);
            assertIsLevel(y);

            return this.runtime.ret(new LVal(flowsTo(x.val, y.val), lub (x.lev, y.lev), levels.BOT)) // lub (__sched.pc, lub(x.lev, y.lev))))
        })


        newlabel = mkBase((arg) => {
            assertIsUnit(arg);
            let levid = uuidv4().toString()
            return this.runtime.ret(this.runtime.mkLabel(levid));
        })       
    }
}


