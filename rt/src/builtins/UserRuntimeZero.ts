'use strict'
import { runtimeEquals } from '../EqualityChecker'
import { isListFlagSet, isTupleFlagSet, mkTuple, mkList } from '../ValuesUtil'
import { LVal, LValCopyAt, LCopyVal } from '../Lval'
import { Nil, Cons, RawList } from '../RawList'
import { loadLibsAsync } from '../loadLibsAsync';
import options from '../options'
import { BaseFunctionWithExplicitArg, ServiceFunction } from '../BaseFunction'
import { Atom } from '../Atom'
import { assertIsString, assertPairAreNumbers, assertPairAreStringsOrNumbers, assertIsBoolean, assertIsListOrTuple, assertIsNumber, rawAssertIsList} from '../Asserts'
import { __unit } from '../UnitVal'
import { RuntimeInterface } from '../RuntimeInterface';
import { Record } from '../Record'
import { TroupeType } from '../TroupeTypes'
import { RawClosure } from '../RawClosure'
import __unitbase from '../UnitBase'
import { Thread } from '../Thread'

// import { builtin_sandbox } from './builtins/sandox'

export type Constructor<T = {}> = new (...args: any[]) => T;


const levels = options;
const {lub, lubs} = levels

class RtEnv {
    _is_rt_env: boolean;
    constructor() {
        this._is_rt_env = true;
    }
}

class LibEnv {
    ret: any;
    _is_rt_env: boolean
    constructor() {
        this._is_rt_env = false;
        this.ret = null;
    }
}


export function mkBase(f,name=null) {
    return new LVal(BaseFunctionWithExplicitArg(f,name), levels.BOT);
}

export function mkService(f, name = null) {
    return new LVal(ServiceFunction(f, name), levels.BOT);
}

export class UserRuntimeZero {
    runtime: RuntimeInterface
    
    mkuuid: any
    mkRecord = Record.mkRecord    
    mkTuple = mkTuple
    mkList = mkList
    sandbox: any
    sleep: any

    Env = RtEnv
    RawClosure = RawClosure    
    constructLVal =  (x,y,z) => new LVal (x,y,z) 
    mkVal : (x:any) => LVal = this.default_mkVal
    mkValPos : (x:any, pos:string) => LVal = this.default_mkValPos
    __unit = __unit
    __unitbase = __unitbase
    Atom = Atom

    constructor(runtime:RuntimeInterface) {                  
        this.runtime = runtime
    }


    ret (x) {
        this.runtime.ret (x)
    }

    eq(x, y) {
        return runtimeEquals(x, y)
    }

    join (...xs) {
        if (this.runtime.$t._isDataBoundByPC) {
            return this.runtime.$t.pc 
        } 
        return lub.apply (null, xs)
    }

    wrap_block_rhs (x) {
        if (this.runtime.$t._isDataBoundByPC) {
            return this.runtime.$t.bl 
        } else {
            return x;
        }
        
    }


    neq(x, y) {
        let b = runtimeEquals(x, y);
        b.val = !b.val;
        return b;
    }

    stringConcat(x, y) {
        assertIsString(x);
        assertIsString(y);
        return new LVal((x.val + y.val), lub(x.lev, y.lev), levels.BOT);
    }


    plus(x, y) {
        assertPairAreNumbers(x, y);
        return new LVal((x.val + y.val), lub(x.lev, y.lev), levels.BOT)
    }

    minus(x, y) {
        assertPairAreNumbers(x, y);
        let rv = new LVal((x.val - y.val), lub(x.lev, y.lev), levels.BOT)
        return rv;
    }

    mult(x, y) {
        assertPairAreNumbers(x, y);
        return new LVal((x.val * y.val), lub(x.lev, y.lev), levels.BOT)
    }

    div(x, y) {
        assertPairAreNumbers(x, y);
        return new LVal((x.val / y.val), lub(x.lev, y.lev), levels.BOT)
    }

    intdiv(x, y) {
        return Math.trunc(x / y) ;
        // assertPairAreNumbers(x, y);
        // return new LVal(Math.trunc(x.val / y.val), lub(x.lev, y.lev), levels.BOT)
    }

    mod(x, y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val % y.val, lub(x.lev, y.lev), levels.BOT)
    }

    binand (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val & y.val, lub(x.lev, y.lev), levels.BOT)
    }

    binor (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val | y.val, lub(x.lev, y.lev), levels.BOT)
    }

    binxor (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val ^ y.val, lub(x.lev, y.lev), levels.BOT)
    }

    shiftleft (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val << y.val, lub(x.lev, y.lev), levels.BOT)
    }

    shiftright (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val >> y.val, lub(x.lev, y.lev), levels.BOT)
    }

    zeroshiftright (x,y) {
        assertPairAreNumbers(x, y);
        return new LVal(x.val >>> y.val, lub(x.lev, y.lev), levels.BOT)
    }





    le(x, y) {
        assertPairAreStringsOrNumbers(x, y);
        return new LVal((x.val <= y.val), lub(x.lev, y.lev), levels.BOT)
    }

    lt(x, y) {
        assertPairAreStringsOrNumbers(x, y);
        return new LVal((x.val < y.val), lub(x.lev, y.lev), levels.BOT)
    }

    ge(x, y) {
        assertPairAreStringsOrNumbers(x, y);
        return new LVal((x.val >= y.val), lub(x.lev, y.lev), levels.BOT)
    }

    gt(x, y) {
        assertPairAreStringsOrNumbers(x, y);
        return new LVal((x.val > y.val), lub(x.lev, y.lev), levels.BOT)
    }

    and(x, y) {
        assertIsBoolean(x);
        assertIsBoolean(y);
        return new LVal((x.val && y.val), lub(x.lev, y.lev), levels.BOT)
    }

    or(x, y) {
        assertIsBoolean(x);
        assertIsBoolean(y);
        return new LVal((x.val || y.val), lub(x.lev, y.lev), levels.BOT)
    }

    index(x:LVal, y:LVal) {
        assertIsListOrTuple(x);
        assertIsNumber(y);
        let z = x.val._troupeType == TroupeType.TUPLE ? x.val[y.val] : x.val.index(y.val)
        return new LVal(z.val, lub(lub(x.lev, y.lev), z.lev), z.tlev)
    }

    raw_index (x,y) {
        return x[y];
    }

    raw_islist(x) {
        return isListFlagSet(x)
    }

    islist(x) {
        return new LVal(isListFlagSet(x.val), x.lev, x.tlev)
    }

    istuple(x) {
        return new LVal(Array.isArray(x.val) && isTupleFlagSet(x.val), x.lev, x.tlev)
    }

    raw_istuple(x) {
        return (x._troupeType == TroupeType.TUPLE)
    }

    getField (x:Record,f:string) {
        return x.getField (f)
        /*
        // assertIsRecordWithField (x,f)        
        let v = x.val.getField (f)
        let l = lub(x.lev, v.lev)
        return new LVal (v.val, l, l)
        */
    }

    hasField (x:Record,f:string) {
        return x.hasField (f)
        // return new LVal ( x.val._troupeType == TroupeType.RECORD && 
                        //   x.val.hasField(f.val), lub(x.lev, f.lev), levels.BOT)
    }

    isRecord (x:LVal) {
        return (x._troupeType == TroupeType.RECORD)
    }

    withRecord (r:Record, fields) {
        // assertIsRecord(r)
        return Record.mkWithRecord(r,fields)
    }

    cons (a, b) {
        rawAssertIsList(b) // 2019-03-07: AA; consider forcing the elements of the list to be of the same type (unless nil)
        return new Cons (a,b)
        // return new LVal(new Cons(a, b.val), b.lev, levels.BOT)
    }

    length(x) {
        assertIsListOrTuple(x);
        return new LVal(x.val.length, x.lev, levels.BOT)
    }

    raw_length (x) {
        return x.length
    }


    head(x) {
        rawAssertIsList(x)
        return x.head;
        // return new LVal(y.val, lub(y.lev, x.lev), lub(y.tlev, x.lev))
    }

    tail = function (x) {
        rawAssertIsList(x)
        return x.tail
    }
    
    getVal = function (x) {
        return x.val
    }

    branch = function (x) {
        this.runtime.$t.setBranchFlag()
        this.runtime.$t.raiseCurrentThreadPC(x.lev);
    }

    push(x, frameSize) {
        this.runtime.$t.pushFrame(x, frameSize);
    }

    assertOrError(x) {
        this.runtime.$t.raiseBlockingThreadLev(x.lev);
    }


    default_mkVal(x) {
        return this.runtime.$t.mkVal(x)        
    }

    default_mkValPos(x,p) {
        return this.runtime.$t.mkValPos(x, p)
    }

    mkCopy (x):LVal {
        return this.runtime.$t.mkCopy(x)
    }



    libLoadingPseudoThread = new Thread(null, null, null, __unit, levels.BOT, levels.BOT, null, this, null);
    savedThread =  null ;// this.runtime.__sched.__currentThread;
    setLibloadMode() {
        this.mkVal = (x) => new LVal(x, levels.BOT);
        this.mkValPos = (x, pos) => new LVal(x, levels.BOT, levels.BOT, pos);
        this.Env = LibEnv;
        this.savedThread = this.runtime.__sched.__currentThread;
        this.runtime.__sched.__currentThread = this.libLoadingPseudoThread;
    }


    setNormalMode() {
        this.mkVal = this.default_mkVal;
        this.mkValPos = this.default_mkValPos
        this.Env = RtEnv;
        this.runtime.__sched.__currentThread = this.savedThread;
    }

    // tailcall(lff, arg) {    
    //     this.runtime.tailcall (lff, arg)
    // }

    // raw_tailcall(x) {
    //     this.runtime.__sched.tailToTroupeFun_raw (x);
    // }


    loadLib(lib, decl, obj) {
        // load the lib from the linked data structure
        let r = obj.libs[lib + "." + decl];  
        let rv = this.mkVal(r);
        // rt_debug("loading lib " + decl);
        return rv;
    }

    async linkLibs (f) {
        await loadLibsAsync(f, this)
    }

    mkLabel(x) {
        // debug ("mkLabel", x, x === "secret");
        return levels.mkLevel (x)
        // return new LVal(levels.mkLevel(x), this.runtime.$t.pc);
    }

    raisedTo (x,y) {
        this.runtime.$t.invalidateSparseBit()        
        return lub (x,y) // just a wrapper around join ; 2021-05-07; AA
    }
    
    // raisedTo (x, y) {
    //     assertIsLevel(y)
    //     return new LCopyVal(x, lub(lub(x.lev, y.val), y.lev), lubs([x.tlev, y.tlev, this.runtime.$t.pc]) )
    // }

    unaryMinus (x) {
        assertIsNumber(x);
        return new LVal(-x.val, x.lev, levels.BOT)
    }

    errorPos (x, pos) {
        if (pos != '') {
            this.runtime.$t.threadError(x.val + " at " + pos);
        } else {
            this.runtime.$t.threadError(x.val);
        }    
    }


    rawErrorPos (x, pos) {
        if (pos != '') {
            this.runtime.$t.threadError(x+ " at " + pos);
        } else {
            this.runtime.$t.threadError(x);
        }    
    }
    

}



