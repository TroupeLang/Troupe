import { Thread, Capability } from './Thread.mjs';

import { isListFlagSet, isTupleFlagSet } from './ValuesUtil.mjs';
import * as proc from './process.mjs';
const ProcessID = proc.ProcessID;
import { AbstractLevel } from './AbstractLevel.mjs';
import { Level } from './Level.mjs';
import { Authority } from './Authority.mjs'
import * as levels from './Level.mjs';
import { TroupeType } from './TroupeTypes.mjs';
const actsFor = levels.actsFor;

import { getRuntimeObject } from './SysState.mjs';
import { __nodeManager } from './NodeManager.mjs';
import { TroupeAggregateRawValue, TroupeRawValue } from './TroupeRawValue.mjs';
import { ErrorKind } from './TroupeError.mjs';
// import { LVal } from './Lval';

/**
 * Identifies where an assertion is being called from.
 * Used to determine error reporting behavior.
 */
export enum AssertionSource {
    /** Assertion called from within a runtime built-in function (default) */
    AssertInBuiltIn,
    /** Assertion called from generated user code */
    AssertInUserCode
}

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

/**
 * Maps AssertionSource to the corresponding ErrorKind.
 */
function sourceToErrorKind(source: AssertionSource): ErrorKind {
    return source === AssertionSource.AssertInUserCode
        ? ErrorKind.DynTypeError
        : ErrorKind.BuiltInArgsTypeMismatch;
}

/**
 * Reports an error from an assertion failure.
 * @param msg The error message
 * @param source Where the assertion was called from (determines error kind)
 */
function err(msg: string, source: AssertionSource) {
    const errorKind = sourceToErrorKind(source);
    _thread().threadError(msg, false, null, errorKind);
}
export function assertIsAtom (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev)
    if (x.val._troupeType != TroupeType.ATOM ) {
        err ("value " + __stringRep(x) + " is not an atom", source)
    }
}

export function assertIsBigInt (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev)
    if (x.val._troupeType != TroupeType.BIGINT ) {
        err ("value " + __stringRep(x) + " is not a bigint", source)
    }
}

export function rawAssertIsNumber (x, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (typeof x != 'number') {
        err("value " + __stringRep(x) + " is not a number", source)
    }
}

export function assertIsNumber(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev)
    if (typeof x.val != 'number') {
        err("value " + __stringRep(x) + " is not a number", source)
    }
}

export function assertIsBoolean(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'boolean') {
        err("value " + __stringRep(x) + " is not a boolean", source)
    }
}

export function rawAssertIsBoolean(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (typeof x != 'boolean') {
        err("value " + __stringRep(x) + " is not a boolean", source)
    }
}

export function assertIsFunction(x: any, internal = false, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    rawAssertIsFunction (x.val, internal, source)
}

export function rawAssertIsFunction(x, internal = false, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (x._troupeType != TroupeType.CLOSURE) {
        const errorKind = sourceToErrorKind(source);
        _thread().threadError("value " + __stringRep(x) + " is not a function", internal, null, errorKind)
    }
}


export function assertIsLocalObject(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (x.val._troupeType != TroupeType.LOCALOBJECT) {
        err("value " + __stringRep(x) + " is not a local object", source)
    }
}

export function assertIsHandler(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (x.val._troupeType != TroupeType.CLOSURE) {
        err("value " + __stringRep(x) + " is not a handler", source)
    }
}

export function assertIsUnit(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!x.val._is_unit) {
        err("value " + __stringRep(x) + " is not unit", source)
    }
}


export function assertIsListOrTuple(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.lev);;
    if (!((isListFlagSet(x.val) || isTupleFlagSet(x.val)))) {
        err("value " + __stringRep(x) + " is not a list or tuple", source)
    }
}

export function assertIsList(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.lev);;
    rawAssertIsList(x.val, source)
}

export function rawAssertIsList (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (!isListFlagSet(x)) {
        err("value " + __stringRep(x) + " is not a list", source)
    }
}

export function assertIsNTuple(x: any, n: number, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.lev);
    if (!(Array.isArray(x.val) && isTupleFlagSet(x.val) && x.val.length == n)) {
        err("value " + __stringRep(x) + " is not a " + n + "-tuple", source)
    }
}

/**
 * Assert x is a tuple with arity in the given set.
 * Raises blocking level like assertIsNTuple.
 */
export function assertIsTupleWithArity(
    x: any,
    allowedArities: number[],
    source: AssertionSource = AssertionSource.AssertInBuiltIn
) {
    _thread().raiseBlockingThreadLev(x.lev);  // Critical: preserve blocking level
    if (!(Array.isArray(x.val) && isTupleFlagSet(x.val))) {
        err("value " + __stringRep(x) + " is not a tuple", source);
    }
    if (!allowedArities.includes(x.val.length)) {
        const aritiesStr = allowedArities.join(" or ");
        err(`expected ${aritiesStr}-tuple, got ${x.val.length}-tuple`, source);
    }
}


export function assertIsNTupleR3 (x:TroupeRawValue, lev:Level, tlev:Level, n:number, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(lev);
    if (!(Array.isArray(x) && isTupleFlagSet(x) && x.length == n)) {
        err("value " + __stringRep(x) + " is not a " + n + "-tuple", source)
    }
}

export function rawAssertIsTuple (x, source: AssertionSource = AssertionSource.AssertInBuiltIn)  {
    if (!(Array.isArray(x) && isTupleFlagSet(x) )) {
        err("value " + __stringRep(x) + " is not a tuple", source)
    }
}

/**
 * Assumes `x` is a tuple and asserts it has at least length `n`.
 */
export function rawAssertTupleLengthGreaterThan (x, n: number, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (x.length <= n) {
        err("Index out of bounds: tuple " + __stringRep(x) + " does not have length more than " + n, source)
    }
}


export function rawAssertRecordHasField (x, field: string, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (!x.hasField(field)) {
        err (`record ${__stringRep(x)} does not have field \'${field}\'`, source)
    }
}


export function assertIsRecord (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.lev);
    if (x.val._troupeType != TroupeType.RECORD) {
        err (`value ${__stringRep(x)} is not a record`, source)
    }
}

export function rawAssertIsRecord (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (x._troupeType != TroupeType.RECORD) {
        err (`value ${__stringRep(x)} is not a record`, source)
    }
}

export function assertIsString(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'string') {
        err("value " + __stringRep(x) + " is not a string", source)
    }
}

export function rawAssertIsString(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (typeof x != 'string') {
        err("value " + __stringRep(x) + " is not a string", source)
    }
}

export function rawAssertNotZero(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (x === 0) {
        err("Division by zero error", source)
    }
}


export function assertIsNode(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (typeof x.val != 'string') {
        err("value " + __stringRep(x) + " is not a node string", source) // todo: check for it being a proper nodeid format?
    }
    if (x.val.startsWith("@")) {
        if (!__nodeManager.aliases[x.val.substring(1)]) {
            err(`${x.val} is not a defined alias`, source)
        }
    }
}

export function assertIsProcessId(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof ProcessID)) {
        err("value " + __stringRep(x) + " is not a process id", source)
    }
}


export function assertIsCapability(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof Capability)) {
        err("value " + __stringRep(x) + " is not a capability of lowering the mailbox clearance", source)
    }
}

export function assertIsLevel(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof AbstractLevel)) {
        err("value " + __stringRep(x) + " is not a level", source);
    }
}

export function rawAssertIsLevel (x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (!(x instanceof AbstractLevel)) {
        err("value " + __stringRep(x) + " is not a level", source);
    }
}

export function assertIsRootAuthority(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    let isTop = actsFor(x.val.authorityLevel, levels.ROOT);
    if (!isTop) {
        let errorMessage =
            "Provided authority is not ROOT\n" +
            ` | level of the provided authority: ${x.val.authorityLevel.stringRep()}`
        err(errorMessage, source);
    }
}

export function assertIsAuthority(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val instanceof Authority)) {
        err("value " + __stringRep(x) + " is not a authority", source);
    }
}

export function assertIsAuthorityR3(x, lev, tlev, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x instanceof Authority)){
        err("value " + __stringRep(x) + " is not a authority", source);
    }
}

export function assertIsEnv(x: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    _thread().raiseBlockingThreadLev(x.tlev);
    if (!(x.val._is_rt_env)) {
        err("value " + __stringRep(x) + " is not an environment", source);
    }
}

export function assertNormalState(s: string, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (!_thread().handlerState.isNormal()) {
        err("invalid handler state in " + s + " -- side effects are prohbited in handler pattern matching or sandboxed code", source)
    }
}

export function assertDeclassificationAllowed(s: string, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    if (!_thread().handlerState.declassificationAllowed()) {
        err("invalid handler state in " + s + ": declassification prohibited in handler pattern matching", source)
    }
}


export function assertPairAreNumbers(x: any, y: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    assertIsNumber(x, source);
    assertIsNumber(y, source);
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

export function rawAssertPairsAreStringsOrNumbers (x: any, y: any, source: AssertionSource = AssertionSource.AssertInBuiltIn) {
    switch (typeof x) {
        case 'number': rawAssertIsNumber(y, source); break
        case 'string': rawAssertIsString(y, source); break
        default: err("value " + __stringRep(x) + " is not a number or a string", source)
        // default: err("values " + __stringRep(x) + " and " + __stringRep(y) + " are of different types", source)
    }
}
