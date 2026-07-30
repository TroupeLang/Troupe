import {TroupeType} from './TroupeTypes.mjs'
import {TroupeAggregateRawValue} from './TroupeRawValue.mjs'
import {LVal, listStringRep} from './Lval.mjs'
import { Level } from './Level.mjs'
import * as levels from './Level.mjs'


export abstract class RawList implements TroupeAggregateRawValue {    
    _troupeType = TroupeType.LIST;
    isList = true ;
    isNil : boolean

    constructor() {
        
    }

    abstract toArray (): LVal [];
    abstract get length() : number;
    abstract get head () : LVal 
    abstract get tail () : RawList 
    abstract index (j:number) : LVal
    
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

    index (j:number) {
        throw new Error ("index: empty list")
        return null
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

    index (j:number) {
        if (j == 0) {
            return this._head 
        } else {
            return this._tail.index(j - 1)
        }
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

    // Walks the spine iteratively: recursing here costs one JS stack frame per
    // element, so a list longer than the engine's stack overflows it. The
    // length is cached on every cons cell, so the result array is sized once.
    toArray () {
        const a : LVal [] = new Array (this._length);
        let x : RawList = this;
        for (let j = 0; j < a.length; j ++) {
            a[j] = x.head;
            x = x.tail;
        }
        return a;
    }
}
