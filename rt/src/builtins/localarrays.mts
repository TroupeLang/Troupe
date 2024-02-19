import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as options from '../options.mjs'
import { assertIsUnit, assertIsNumber, assertIsNTuple, assertIsLevel, assertIsLocalObject } from '../Asserts.mjs'
import { LocalObject } from '../LocalObject.mjs';
import { __unit } from '../UnitVal.mjs';
let {lub, flowsTo} = options;



export function BuiltinLocalArrays <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        arrayCreate = mkBase((arg) => {
            assertIsNTuple(arg, 3)
            let lev = arg.val[0]
            let size = arg.val[1]
            let def = arg.val[2]
            assertIsNumber (size)
            assertIsLevel (lev)
            let l1 = lub(lev.lev, size.lev, this.runtime.$t.bl)
            if (!flowsTo (l1, lev.val)) {
                this.runtime.$t.threadError (`The declared array level is too low:\n` + 
                                             `| array declared level is: ${lev.val.stringRep()}\n` +
                                             `| the level of the information affecting the array creation: ${l1.stringRep()}`)
            }
            let rawArray = new Array (size.val)
            let obj = { size, lev: lev.val, rawArray, def }
            return this.runtime.ret (this.mkVal (new LocalObject(obj)))
        })

        arrayGet = mkBase((arg) => {
            assertIsNTuple(arg, 2)
            let obj = arg.val[0]
            let idx = arg.val[1]
            assertIsNumber (idx)
            let {lev, size, rawArray, def } = obj.val._value
            assertIsLocalObject(obj)
            this.runtime.$t.raiseBlockingThreadLev(lub(idx.lev, size.lev))

            if (idx.val >= size.val) {
                this.runtime.$t.threadError("Array index out of bounds")
            }
            let v = rawArray [idx.val]
            let u = v || def  // return default if the array element is not set 
            return this.runtime.ret ( new LVal (u.val, lub (this.runtime.$t.bl, u.lev, lev, obj.lev)))
        })

        arraySet = mkBase((arg) => {
            assertIsNTuple(arg, 3)
            let obj = arg.val[0]
            let idx = arg.val[1]
            let value = arg.val[2]
            assertIsLocalObject(obj)
            assertIsNumber (idx)
            let {lev, size, rawArray} = obj.val._value
            this.runtime.$t.raiseBlockingThreadLev(lub(size.lev,idx.lev,obj.lev))
            let l1 = lub(idx.lev, obj.lev, this.runtime.$t.bl)
            if (!flowsTo (l1, lev)) {
                this.runtime.$t.threadError ("Information influencing the write to this array is too high:\n" + 
                                             `| array declared level is ${lev.stringRep()}\n` + 
                                             `| the level of information affecting the write is ${l1.stringRep()}`)
            }
            if (idx.val >= size.val) {
                this.runtime.$t.threadError("Array index out of bounds")
            }
            rawArray [idx.val] = value
            this.runtime.ret (__unit)
        })
    }
}

