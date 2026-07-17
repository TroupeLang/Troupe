import { TroupeType } from "./TroupeTypes.mjs"
import { TroupeRawValue } from "./TroupeRawValue.mjs";
import * as levels from './Level.mjs'

/** A Troupe bigint value: a JavaScript bigint boxed with a type tag.
 *
 *  Boxing is required because a JS bigint is a primitive and cannot carry
 *  the _troupeType property; LVal construction dispatches on that property
 *  (and its fallback for unboxed primitives handles only number, boolean,
 *  and string). The label of a bigint is carried by the enclosing LVal,
 *  like every other base value.
 */
export class TroupeBigInt implements TroupeRawValue {
  value: bigint
  _troupeType = TroupeType.BIGINT
  dataLevel = levels.BOT

  constructor (value: bigint) {
    this.value = value
  }

  // Printed in literal form (e.g. 5n) so bigints are distinguishable from
  // numbers in output and error messages; string conversion via
  // bigToString/BigInt.show yields the plain decimal digits.
  stringRep (_omitLevels = false) {
    return this.value.toString() + "n"
  }
}
