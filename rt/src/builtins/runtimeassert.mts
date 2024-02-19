import { assertIsFunctionRaw, rawAssertIsBoolean, rawAssertIsList, rawAssertIsNumber, rawAssertIsRecord, rawAssertIsString, rawAssertIsTuple, rawAssertPairsAreStringsOrNumbers, rawAssertIsLevel } from '../Asserts.mjs'
import {UserRuntimeZero, Constructor } from './UserRuntimeZero.mjs'


export function RuntimeAssert <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {

        rawAssertIsNumber = rawAssertIsNumber
        rawAssertIsBoolean = rawAssertIsBoolean
        rawAssertIsString = rawAssertIsString
        rawAssertIsList = rawAssertIsList
        rawAssertIsFunction = assertIsFunctionRaw
        rawAssertIsRecord = rawAssertIsRecord
        rawAssertIsTuple = rawAssertIsTuple
        rawAssertPairsAreStringsOrNumbers = rawAssertPairsAreStringsOrNumbers
        rawAssertIsLevel = rawAssertIsLevel 
    }
}
