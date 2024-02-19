import { Thread, Capability } from './Thread.mjs';

import { isListFlagSet, isTupleFlagSet } from './ValuesUtil.mjs';
import * as proc from './process.mjs';
const ProcessID = proc.ProcessID;
import { Level } from './Level.mjs';
import { Authority } from './Authority.mjs'
import * as levels from './options.mjs'; 
import { TroupeType } from './TroupeTypes.mjs';
const flowsTo = levels.flowsTo;

import { getRuntimeObject } from './SysState.mjs';
import { __nodeManager } from './NodeManager.mjs';
import { TroupeAggregateRawValue, TroupeRawValue } from './TroupeRawValue.mjs';
// import { LVal } from './Lval';

function _thread() {
    return getRuntimeObject().__sched.__currentThread
}

function __stringRep (v) {
    if (v.stringRep) {
        return v.stringRep()
    } else {
        let t=""
        if (typeof v === 'string') {
            t = "\"" + v.toString() + "\""
        } else {
            t = v.toString();
        }
        return t
    }
}

let err = x => _thread().threadError(x)
export function assertIsAtom (x: any) {
    _thread().raiseBlockingThreadLev(x.tlev)
    if (x.val._troupeType != TroupeType.ATOM ) {
        err ("value " + __stringRep(x) + " is not an atom")        
    }
}

export function rawAssertIsNumber (x) {
    if (typeof x != 'number') {
        err("value " + __stringRep(x) + " is not a number")
    }
}

export function assertIsNumber(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev)
    if (typeof x.val != 'number') {
        err("value " + __stringRep(x) + " is not a number")
    }
}

export function assertIsBoolean(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'boolean') {
        err("value " + __stringRep(x) + " is not a boolean")
    }
}

export function rawAssertIsBoolean(x:any) {
    if (typeof x != 'boolean') {
        err("value " + __stringRep(x) + " is not a boolean")
    }
}

export function assertIsFunction(x: any, internal = false) {
    _thread().raiseBlockingThreadLev(x.tlev);
    rawAssertIsFunction (x.val, internal)
}

export function rawAssertIsFunction(x, internal = false) {
    if (x._troupeType != TroupeType.CLOSURE) {
        _thread().threadError("value " + __stringRep(x) + " is not a function", internal)
    }
}


export function assertIsLocalObject(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (x.val._troupeType != TroupeType.LOCALOBJECT) {
        err("value " + __stringRep(x) + " is not a local object")
    }
}

export function assertIsHandler(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (x.val._troupeType != TroupeType.CLOSURE) {
        err("value " + __stringRep(x) + " is not a handler")
    }
}

export function assertIsUnit(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!x.val._is_unit) {
        err("value " + __stringRep(x) + " is not unit")
    }
}


export function assertIsListOrTuple(x: any) {
    _thread().raiseBlockingThreadLev(x.lev);;
    if (!((isListFlagSet(x.val) || isTupleFlagSet(x.val)))) {
        err("value " + __stringRep(x) + " is not a list or tuple")
    }
}

export function assertIsList(x: any) {
    _thread().raiseBlockingThreadLev(x.lev);;
    rawAssertIsList(x.val)
}

export function rawAssertIsList (x:any) {
    if (!isListFlagSet(x)) {
        err("value " + __stringRep(x) + " is not a list")
    }
}

export function assertIsNTuple(x: any, n: number) {
    _thread().raiseBlockingThreadLev(x.lev);
    if (!(Array.isArray(x.val) && isTupleFlagSet(x.val) && x.val.length == n)) {
        err("value " + __stringRep(x) + " is not a " + n + "-tuple")
    }
}


export function assertIsNTupleR3 (x:TroupeRawValue, lev:Level, tlev:Level, n:number) {
    _thread().raiseBlockingThreadLev(lev);
    if (!(Array.isArray(x) && isTupleFlagSet(x) && x.length == n)) {
        err("value " + __stringRep(x) + " is not a " + n + "-tuple")
    }
}

export function rawAssertIsTuple (x)  {
    if (!(Array.isArray(x) && isTupleFlagSet(x) )) {
        err("value " + __stringRep(x) + " is not a tuple")
    } 
}

/**
 * Assumes `x` is a tuple and asserts it has at least length `n`.
 */
export function rawAssertTupleLengthGreaterThan (x, n: number) {
    if (x.length <= n) {
        err("Index out of bounds: tuple " + __stringRep(x) + " does not have length more than " + n)
    }
}


export function rawAssertRecordHasField (x, field: string) {
    if (!x.hasField(field)) {
        err (`record ${__stringRep(x)} does not have field \'${field}\'`)
    }
}


export function assertIsRecord (x: any) {
    _thread().raiseBlockingThreadLev(x.lev);
    if (x.val._troupeType != TroupeType.RECORD) {
        err (`value ${__stringRep(x)} is not a record`)
    }
}

export function rawAssertIsRecord (x: any) {
    if (x._troupeType != TroupeType.RECORD) {
        err (`value ${__stringRep(x)} is not a record`)
    }
}

export function assertIsString(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'string') {
        err("value " + __stringRep(x) + " is not a string")
    }
}

export function rawAssertIsString(x:any) {
    if (typeof x != 'string') {
        err("value " + __stringRep(x) + " is not a string")
    } 
}


export function assertIsNode(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'string') {
        err("value " + __stringRep(x) + " is not a node string") // todo: check for it being a proper nodeid format?
    }
    if (x.val.startsWith("@")) {
        if (!__nodeManager.aliases[x.val.substring(1)]) {
            err(`${x.val} is not a defined alias`)
        }
    }
}

export function assertIsProcessId(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof ProcessID)) {
        err("value " + __stringRep(x) + " is not a process id")
    }
}


export function assertIsCapability(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof Capability)) {
        err("value " + __stringRep(x) + " is not a capability of lowering the mailbox clearance")
    }
}

export function assertIsLevel(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof Level)) {
        err("value " + __stringRep(x) + " is not a level");
    }
}

export function rawAssertIsLevel (x:any) {
    if (!(x instanceof Level)) {
        err("value " + __stringRep(x) + " is not a level");
    }
}
export function assertIsTopAuthority(x: any) {
    let isTop = flowsTo(levels.TOP, x.val.authorityLevel);
    if (!isTop) {
        let errorMessage =
            "Provided authority is not TOP\n" +
            ` | level of the provided authority: ${x.val.authorityLevel.stringRep()}`
        err(errorMessage);
    }
}

export function assertIsAuthority(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof Authority)) {
        err("value " + __stringRep(x) + " is not a authority");
    }
}

export function assertIsAuthorityR3(x, lev, tlev) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x instanceof Authority)){
        err("value " + __stringRep(x) + " is not a authority");
    }
}

export function assertIsEnv(x: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val._is_rt_env)) {
        err("value " + __stringRep(x) + " is not an environment");
    }
}

export function assertNormalState(s: string) {
    if (!_thread().handlerState.isNormal()) {
        err("invalid handler state in " + s + " -- side effects are prohbited in handler pattern matching or sandboxed code")
    }
}

export function assertDeclassificationAllowed(s: string) {
    if (!_thread().handlerState.declassificationAllowed()) {
        err("invalid handler state in " + s + ": declassification prohibited in handler pattern matching")
    }
}


export function assertPairAreNumbers(x: any, y: any) {
    assertIsNumber(x);
    assertIsNumber(y);
}

/*
export function assertPairAreStringsOrNumbers(x: any, y: any) {
    _thread().raiseBlockingThreadLev(x.tlev);
    switch (typeof x.val) {
        case 'number': assertIsNumber(y); break;
        case 'string': assertIsString(y); break;
        default: err("values " + __stringRep(x) + " and " + __stringRep(y) + " are of different types")
    }
}
*/

export function rawAssertPairsAreStringsOrNumbers (x:any, y:any) {
    switch (typeof x) {
        case 'number': rawAssertIsNumber(y); break 
        case 'string': rawAssertIsString(y); break 
        default: err("value " + __stringRep(x) + " is not a number or a string")
        // default: err("values " + __stringRep(x) + " and " + __stringRep(y) + " are of different types")
    }
}
