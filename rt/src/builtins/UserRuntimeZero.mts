'use strict'
import { runtimeEquals } from '../EqualityChecker.mjs'
import { isListFlagSet, isTupleFlagSet, mkTuple, mkList } from '../ValuesUtil.mjs'
import { LVal, LValCopyAt, LCopyVal } from '../Lval.mjs'
import { Nil, Cons, RawList } from '../RawList.mjs'
import { loadLibsAsync } from '../loadLibsAsync.mjs';
import * as levels from '../options.mjs'
import { BaseFunctionWithExplicitArg, ServiceFunction } from '../BaseFunction.mjs'
import { Atom } from '../Atom.mjs'
import { __unit } from '../UnitVal.mjs'
import { RuntimeInterface } from '../RuntimeInterface.mjs';
import { Record } from '../Record.mjs'
import { TroupeType } from '../TroupeTypes.mjs'
import { RawClosure } from '../RawClosure.mjs'
import { __unitbase } from '../UnitBase.mjs'
import { Thread } from '../Thread.mjs'
import { TroupeRawValue } from '../TroupeRawValue.mjs'
import { RawTuple } from '../RawTuple.mjs'
import { Level } from '../Level.mjs'

// import { builtin_sandbox } from './builtins/sandox'

export type Constructor<T = {}> = new (...args: any[]) => T;


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
    return BaseFunctionWithExplicitArg(f,name)
}

export function mkService(f, name = null) {
    return ServiceFunction(f, name)
}

/**
 * Exposes functions available to generated code, used by the Stack2JS module.
 * (TODO: Categorize into assertions, special instructions and general instructions, e.g. using interfaces.
 * Separate from other functions not used by generated code.)
 * Functions used by the generated code are either "SimpleRT" or "ComplexRT" functions (and marked accordingly),
 * where the former just return a plain value which by the generated code is embedded
 * into a labelled value, and the latter return a labelled value, where the label
 * is used by the generated code to compute the eventual label. All operations take plain values as arguments
 * (and not labelled values) unless otherwise noted.
 * Functions marked with "SpecialRT" do not work on values and are special control instructions.
 */
export class UserRuntimeZero {
    runtime: RuntimeInterface
    
    mkuuid: any
    // SimpleRT with array of labelled values as parameter
    mkRecord = Record.mkRecord    
    // SimpleRT with array of labelled values as parameter
    mkTuple = mkTuple
    // SimpleRT with array of labelled values as parameter
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

    // SimpleRT
    raw_join(...xs) : Level {
        return lub.apply (null, xs)
    }

    // SpecialRT
    raw_invalidateSparseBit() {
        this.runtime.$t.invalidateSparseBit()
    }

    // SpecialRT
    rawErrorPos(x: TroupeRawValue, pos: string) {
        if (pos != '') {
            this.runtime.$t.threadError(x + " at " + pos);
        } else {
            this.runtime.$t.threadError("" + x);
        }
    }

    // ComplexRT
    eq(x: TroupeRawValue, y: TroupeRawValue): LVal {
        return runtimeEquals(x, y)
    }

    // ComplexRT
    neq(x: TroupeRawValue, y: TroupeRawValue): LVal {
        let b = runtimeEquals(x, y);
        b.val = !b.val;
        return b;
    }

    // SimpleRT
    intdiv(x: number, y: number): number {
        return Math.trunc(x / y)
    }

    // ComplexRT
    raw_indexTuple(x: TroupeRawValue, y: number): LVal {
        return x[y];
    }

    // SimpleRT
    raw_islist(x: TroupeRawValue): boolean {
        return isListFlagSet(x) // TODO check _troupeType instead?
    }

    // SimpleRT
    raw_istuple(x: TroupeRawValue): boolean {
        return (x._troupeType == TroupeType.TUPLE)
    }

    // ComplexRT
    getField(x: Record, f: string): LVal {
        return x.getField(f)
    }

    // SimpleRT
    hasField(x: Record, f: string): boolean {
        return x.hasField(f)
    }

    // SimpleRT
    isRecord(x: TroupeRawValue): boolean {
        return (x._troupeType == TroupeType.RECORD)
    }

    // SimpleRT
    withRecord(r: Record, fields: Array<[string, LVal]>): Record {
        return Record.mkWithRecord(r, fields)
    }

    // SimpleRT
    cons(a: LVal, b: RawList): RawList {
        return new Cons(a, b)
    }

    // SimpleRT
    raw_listLength(x: RawList): number {
        return x.length
    }

    // SimpleRT
    raw_tupleLength(x: RawTuple): number {
        return x.length
    }

    // ComplexRT
    head(x: RawList): LVal {
        return x.head;
    }

    // SimpleRT
    tail(x: RawList): RawList {
        return x.tail
    }

    // SimpleRT
    mkLabel(x: string): Level {
        return levels.mkLevel(x)
    }

    /**
     * ComplexRT.
     * Lookup a definition from a library.
     * @param lib the library
     * @param decl the declaration to look up
     * @param obj the object to store the result in, under "libs["lib.decl"]"
     * @returns the unlabelled value from the definition
     */
    loadLib(lib: string, decl: string, obj: { libs: { [x: string]: any } }): any {
        // load the lib from the linked data structure
        let r = obj.libs[lib + "." + decl];
        // rt_debug("loading lib " + decl);
        return r;
    }


    /*
     * ==============================================================
     * The remaining functions are not referred to by generated code.
     * ==============================================================
     */
    
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


    async linkLibs (f) {
        await loadLibsAsync(f, this)
    }

    errorPos (x: { val: string }, pos: string) {
        if (pos != '') {
            this.runtime.$t.threadError(x.val + " at " + pos);
        } else {
            this.runtime.$t.threadError(x.val);
        }    
    }

}



