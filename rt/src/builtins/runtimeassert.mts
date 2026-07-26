import { rawAssertIsFunction, rawAssertIsBoolean, rawAssertIsList, rawAssertIsNumber, rawAssertIsRecord, rawAssertIsString, rawAssertIsTuple, rawAssertPairsAreStringsOrNumbers, rawAssertIsLevel, rawAssertTupleLengthGreaterThan, rawAssertRecordHasField, rawAssertNotZero, AssertionSource } from '../Asserts.mjs'
import {UserRuntimeZero, Constructor } from './UserRuntimeZero.mjs'

// Assertions routed through this mixin come from generated user code.
const S = AssertionSource.AssertInUserCode;

// These assertions are invoked from generated user code. The trailing `pos`
// argument carries the source position of the operation so that, on failure,
// the assertion records it in the machine's own position state
// (`lastCallSourcePos`) for error reporting. The compiler emits `pos` only when
// source maps are enabled and the position is meaningful; otherwise it is
// undefined and no position is recorded.
export function RuntimeAssert <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        rawAssertIsNumber = (x, pos = null) => rawAssertIsNumber(x, S, pos)
        rawAssertIsBoolean = (x, pos = null) => rawAssertIsBoolean(x, S, pos)
        rawAssertIsString = (x, pos = null) => rawAssertIsString(x, S, pos)
        rawAssertIsList = (x, pos = null) => rawAssertIsList(x, S, pos)
        // Generated code passes (value, pos); the `internal` flag is not part of the
        // emitted calling convention (the compiler appends the position as the last
        // argument), so it is pinned to false here.
        rawAssertIsFunction = (x, pos = null) => rawAssertIsFunction(x, false, S, pos)
        rawAssertIsRecord = (x, pos = null) => rawAssertIsRecord(x, S, pos)
        rawAssertIsTuple = (x, pos = null) => rawAssertIsTuple(x, S, pos)
        rawAssertTupleLengthGreaterThan = (x, n, pos = null) => rawAssertTupleLengthGreaterThan(x, n, S, pos)
        rawAssertRecordHasField = (x, field, pos = null) => rawAssertRecordHasField(x, field, S, pos)
        rawAssertPairsAreStringsOrNumbers = (x, y, pos = null) => rawAssertPairsAreStringsOrNumbers(x, y, S, pos)
        rawAssertIsLevel = (x, pos = null) => rawAssertIsLevel(x, S, pos)
        rawAssertNotZero = (x, pos = null) => rawAssertNotZero(x, S, pos)
    }
}
