import { rawAssertIsFunction, rawAssertIsBoolean, rawAssertIsList, rawAssertIsNumber, rawAssertIsRecord, rawAssertIsString, rawAssertIsTuple, rawAssertPairsAreStringsOrNumbers, rawAssertIsLevel, rawAssertTupleLengthGreaterThan, rawAssertRecordHasField } from '../Asserts.mjs'
import {UserRuntimeZero, Constructor } from './UserRuntimeZero.mjs'


export function RuntimeAssert <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        rawAssertIsNumber = rawAssertIsNumber
        rawAssertIsBoolean = rawAssertIsBoolean
        rawAssertIsString = rawAssertIsString
        rawAssertIsList = rawAssertIsList
        rawAssertIsFunction = rawAssertIsFunction
        rawAssertIsRecord = rawAssertIsRecord
        rawAssertIsTuple = rawAssertIsTuple
        rawAssertTupleLengthGreaterThan = rawAssertTupleLengthGreaterThan
        rawAssertRecordHasField = rawAssertRecordHasField
        rawAssertPairsAreStringsOrNumbers = rawAssertPairsAreStringsOrNumbers
        rawAssertIsLevel = rawAssertIsLevel 
    }
}
