import {TroupeType} from './TroupeTypes.mjs'
import {TroupeAggregateRawValue} from './TroupeRawValue.mjs'
import {LVal, listStringRep} from './Lval.mjs'
import { Level } from './Level.mjs'
import * as levels from './options.mjs'
import { assertIsRecord } from './Asserts.mjs'


export class Record implements TroupeAggregateRawValue {    
    _troupeType = TroupeType.RECORD
    _dataLevel:  Level = levels.TOP  // TODO compute data level?
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

    constructor(fields: Iterable<readonly [string, LVal]>) {
        this.__obj = new Map (fields)
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

    static mkRecord(fields: Iterable<readonly [string, LVal]>): Record {
        return new Record(fields)
    }  

    static mkWithRecord(r: Record, fields: ConcatArray<[string, LVal]>): Record {
        let a = Array.from(r.__obj)
        let b = a.concat(fields)
        return new Record(b)
    }
}
