'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsNTuple, assertIsRecord, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { Record } from "../Record.mjs";
import { lub } from '../options.mjs';
import { __unit } from '../UnitVal.mjs';

export function BuiltinRecordReflection<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {        
        recordExtend = mkBase((larg) => {
            assertIsNTuple(larg, 3);
            let arg = larg.val;
            assertIsRecord(arg[0]);
            let raw_rec = arg[0].val; 
            assertIsString(arg[1]);
            let new_field_str = arg[1];
            let raw_newfield = new_field_str.val;
            let v = new LVal (arg[2].val, lub (arg[2].lev , new_field_str.lev));
            let new_raw_rec = Record.mkWithRecord (raw_rec, [[raw_newfield, v ]]);
            let ret_val = new LVal (new_raw_rec, lub (this.runtime.$t.pc, larg.lev, new_field_str.lev)); 
            return this.runtime.ret(ret_val);
        })
    }
}
