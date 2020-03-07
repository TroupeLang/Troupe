import {TroupeType} from './TroupeTypes'
import {TroupeAggregateRawValue} from './TroupeRawValue'
import {LVal, listStringRep} from './Lval'
import { Level } from './Level'
import levels = require('./levels/tagsets')


export abstract class RawList implements TroupeAggregateRawValue {    
    _troupeType : TroupeType
    isNil : boolean
    isList: boolean;
    constructor() {
        this._troupeType = TroupeType.LIST;
        this.isList = true;
    }

    abstract toArray (): LVal [];
    abstract get length() : number;
    abstract get head () : LVal 
    abstract get tail () : RawList 
    
    stringRep (omitLevels?: boolean, taintRef?: any) {
        return ("[" + listStringRep(this.toArray(), omitLevels, taintRef) + "]")
    }
    
    static fromArray (a : LVal []) {
        let x = new Nil();
        for (let j = a.length - 1 ; j >= 0; j -- ) {
            x = new Cons(a[j], x);
        }
        return x;
    }

    abstract get dataLevel (): Level 
}

export class Nil extends RawList {
    constructor() {
        super ()
        this.isNil = true;
    }

    get length () {
        return 0;
    }

    get head () {
        throw new Error ("head: empty list")
        return null;
    }

    get tail () {
        throw new Error ("tail: empty list");
        return null;
    }

    toArray () {
        return [];
    }

    get dataLevel (): Level {
        return levels.BOT
    }

}

export class Cons extends RawList {
    _head: LVal
    _tail: RawList 
    _length : number;
    _dataLevel: Level;
    constructor (head: LVal, tail: RawList ) {
        super ();
        this._head = head;
        this._tail = tail;
        this.isNil = false;
        this._length = tail.length + 1 
        this._dataLevel = levels.lub (head.dataLevel, tail.dataLevel)
    }

    get dataLevel (): Level {
        return this._dataLevel
    }

    get head () {
        return this._head
    }

    get tail () {
        return this._tail;
    }

    get length () {
        return this._length;
    }

    toArray () {
        let x = this._tail.toArray();
        x.unshift ( this._head );
        return x;
    }
}
