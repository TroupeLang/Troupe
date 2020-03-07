import {TroupeType} from './TroupeTypes'
import {TroupeRawValue} from './TroupeRawValue'
import {LVal, listStringRep} from './Lval'

export abstract class RawList implements TroupeRawValue {    
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


}

export class Cons extends RawList {
    _head: LVal
    _tail: RawList 
    _length : number;
    constructor (head: LVal, tail: RawList ) {
        super ();
        this._head = head;
        this._tail = tail;
        this.isNil = false;
        this._length = tail.length + 1 
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
