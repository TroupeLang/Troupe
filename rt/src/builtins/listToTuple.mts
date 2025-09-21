'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsList } from '../Asserts.mjs'
import { mkTuple } from '../ValuesUtil.mjs'
import { lub } from '../Level.mjs';

export function BuiltinListToTuple<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        listToTuple = mkBase((larg) => {
            // Assert that the argument is a list
            assertIsList(larg);
            
            // Convert the list to an array of LVals
            let arr = larg.val.toArray();
            
            // Calculate the combined level (join of list level and all element levels)
            let combinedLevel = lub(larg.lev, this.runtime.$t.pc);
            for (let elem of arr) {
                combinedLevel = lub(combinedLevel, elem.lev);
            }
            
            // Create the tuple from the array
            let tuple = mkTuple(arr, false);
            
            // Return the tuple with the combined security level
            return this.runtime.ret(new LVal(tuple, combinedLevel));
        }, "listToTuple")
    }
}