import {TroupeType} from './TroupeTypes.mjs'
import {TroupeRawValue} from './TroupeRawValue.mjs'
import * as levels from './options.mjs'

export class LocalObject implements TroupeRawValue {
    _troupeType : TroupeType
    _value : Object
    dataLevel = levels.TOP // 2021-03-19; AA; consider rethinking what this should be...

    constructor (v:Object) {
        this._troupeType = TroupeType.LOCALOBJECT
        this._value = v
    }

    stringRep (omitLevels?: boolean, taintRef?: any):string { 
        return "LocalObject"
    }
}