import {TroupeType} from './TroupeTypes.mjs'
import {TroupeAggregateRawValue} from './TroupeRawValue.mjs'
import {LVal, listStringRep} from './Lval.mjs'
import { Level } from './Level.mjs'
import * as levels from './options.mjs'
import { assertIsRecord } from './Asserts.mjs'


export class Record implements TroupeAggregateRawValue {    
    _troupeType = TroupeType.RECORD
    _dataLevel:  Level = levels.TOP  // 
    __obj : Map<string, LVal>

    stringRep (omitLevels?: boolean, taintRef?: any) {
        // return ("{" + listStringRep(this.toArray(), omitLevels, taintRef) + "}")
        let s = "{"
        let spaceOrComma = ""
        for (let [k,v] of this.__obj.entries()) {            
            s += spaceOrComma + k + "=" + v.stringRep(omitLevels, taintRef)
            spaceOrComma = ", "
        }
        s += "}"
        return s 
    }

    constructor(_theArray) {
        this.__obj = new Map (_theArray)
    }

    hasField (fieldName:string):boolean {        
        return this.__obj.has(fieldName)
    }

    getField (fieldName:string):LVal {
        return this.__obj.get (fieldName)
    }

    get dataLevel () {
        return this._dataLevel
    }

    static mkRecord(arg) {
        return new Record (arg)
    }  

    static mkWithRecord(r:Record, arg) { 
        let a = Array.from(r.__obj) 
        let b = a.concat (arg)
        return new Record (b)
    }
}
