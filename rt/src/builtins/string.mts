import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsString, assertIsNTuple, assertIsNumber } from '../Asserts.mjs'
import { lub } from '../Level.mjs';


export function BuiltinString <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        charCodeAtWithDefault = mkBase (arg => {
            assertIsNTuple(arg, 3); 
            assertIsString(arg.val[0])
            assertIsNumber(arg.val[1]);

            let s:string = arg.val[0].val 
            let j = arg.val[1].val 
            let lev = lub (arg.lev, arg.val[0].lev, arg.val[1].lev, arg.val[2].lev )

            if ( j >= s.length  || 0 > j ) {
                return this.runtime.ret (new LVal (arg.val[2].val, lev));
            } else {
                return this.runtime.ret (new LVal (s.charCodeAt(j), lev));
            }

        })

        strlen = mkBase (arg => {
            assertIsString(arg);
            let s: string  = arg.val ;
            return this.runtime.ret (new LVal (s.length, arg.lev))
        })

        // Inverse of charCodeAtWithDefault: build a one-character string from a Unicode
        // code point. Accepts the full range 0..0x10FFFF (String.fromCodePoint synthesizes
        // a surrogate pair for astral code points), so callers such as the JSON parser can
        // decode \uXXXX escapes, including surrogate pairs, into real characters. The result
        // carries the argument's label.
        charFromCode = mkBase (arg => {
            assertIsNumber(arg);
            let n: number = arg.val;
            if (!Number.isInteger(n) || n < 0 || n > 0x10FFFF) {
                this.runtime.$t.threadError(
                    `charFromCode: ${n} is not a valid Unicode code point (0..0x10FFFF)`);
                return;
            }
            return this.runtime.ret (new LVal (String.fromCodePoint(n), arg.lev))
        })

        substring = mkBase (arg => {
            assertIsNTuple(arg, 3)
            assertIsString(arg.val[0])
            assertIsNumber(arg.val[1])
            assertIsNumber(arg.val[2])
            let s = arg.val[0].val
            let i = arg.val[1].val 
            let j = arg.val[2].val 
            let s2 = s.substring (i,j) 
            return this.runtime.ret (new LVal(s2, lub ( arg.lev
                                                      , arg.val[0].lev
                                                      , arg.val[1].lev
                                                      , arg.val[2].lev
                                                      )))
        })

        // The index of the first occurrence of `needle` in `s` at or after `from`, in UTF-16
        // code units, or -1 when there is none. JS String.prototype.indexOf semantics: an
        // empty needle answers `from` clamped to [0, length of `s`], and a negative `from`
        // is read as 0. The result carries the join of the three arguments' labels.
        //
        // Named `strIndexOf` rather than `indexOf` because lib/String.trp exports `indexOf`:
        // a builtin of that name is shadowed in every program that imports String unqualified,
        // which is the shadowing that already afflicts `toString`.
        strIndexOf = mkBase (arg => {
            assertIsNTuple(arg, 3)
            assertIsString(arg.val[0])
            assertIsString(arg.val[1])
            assertIsNumber(arg.val[2])
            let s: string = arg.val[0].val
            let needle: string = arg.val[1].val
            let from: number = arg.val[2].val
            return this.runtime.ret (new LVal( s.indexOf (needle, from)
                                             , lub ( arg.lev
                                                   , arg.val[0].lev
                                                   , arg.val[1].lev
                                                   , arg.val[2].lev
                                                   )))
        })
    }
 
}