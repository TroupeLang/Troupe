'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { TroupeBigInt } from '../TroupeBigInt.mjs';
import { Record } from '../Record.mjs';
import { lub } from '../Level.mjs';
import { assertIsNTuple, assertIsNumber, assertIsString, assertIsBigInt } from '../Asserts.mjs'

/** Arbitrary-precision integer operations over the boxed BIGINT type.
 *
 *  Bigints are constructed via `bigFromString`/`bigFromInt` (and the
 *  `123n` literal, which the parser desugars to `bigFromLiteral`), and all
 *  arithmetic goes through these named builtins — the ordinary arithmetic
 *  operators remain number-only. Every result label joins the operand
 *  labels and the current pc: number literals receive their pc join from
 *  generated code, and values produced here must be labeled no lower
 *  (verified against number-literal behavior in a secret branch).
 *
 *  Fallible operations (`bigFromString` on unparsable input, `bigToInt`
 *  beyond exact double range) return Result-shaped records, following the
 *  convention of lib/Result.trp and the SimpleFileIO builtins.
 */

// Largest integer exactly representable as a double, as a bigint.
const MAX_SAFE = BigInt(Number.MAX_SAFE_INTEGER)
const MIN_SAFE = -MAX_SAFE

export function BuiltinBigInt<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {

        private bigOk(v: any, lev: any): LVal {
            const rec = Record.mkRecord([
                ['tag', new LVal('Ok', lev)],
                ['value', new LVal(v, lev)],
            ]);
            return new LVal(rec, lev);
        }

        private bigErr(reason: string, lev: any): LVal {
            const rec = Record.mkRecord([
                ['tag', new LVal('Err', lev)],
                ['error', new LVal(reason, lev)],
            ]);
            return new LVal(rec, lev);
        }

        private bigBinOp(name: string, f: (a: bigint, b: bigint) => any) {
            return mkBase((arg) => {
                assertIsNTuple(arg, 2);
                assertIsBigInt(arg.val[0]);
                assertIsBigInt(arg.val[1]);
                const a: bigint = arg.val[0].val.value;
                const b: bigint = arg.val[1].val.value;
                const lev = lub(arg.lev, arg.val[0].lev, arg.val[1].lev, this.runtime.$t.pc);
                return this.runtime.ret(new LVal(f(a, b), lev));
            }, name)
        }

        // Target of the parser's desugaring of `123n` literals. Total: the
        // lexer guarantees the argument is a valid digit string.
        bigFromLiteral = mkBase((arg) => {
            assertIsString(arg);
            return this.runtime.ret(
                new LVal(new TroupeBigInt(BigInt(arg.val)), lub(arg.lev, this.runtime.$t.pc)));
        }, "bigFromLiteral")

        bigFromString = mkBase((arg) => {
            assertIsString(arg);
            try {
                const v = new TroupeBigInt(BigInt(arg.val.trim()));
                return this.runtime.ret(this.bigOk(v, lub(arg.lev, this.runtime.$t.pc)));
            } catch (e) {
                return this.runtime.ret(
                    this.bigErr("not a valid bigint: " + arg.val, lub(arg.lev, this.runtime.$t.pc)));
            }
        }, "bigFromString")

        bigFromInt = mkBase((arg) => {
            assertIsNumber(arg);
            if (!Number.isSafeInteger(arg.val)) {
                this.runtime.$t.threadError(
                    "bigFromInt requires an exactly-representable integer, got " + arg.val);
            }
            return this.runtime.ret(
                new LVal(new TroupeBigInt(BigInt(arg.val)), lub(arg.lev, this.runtime.$t.pc)));
        }, "bigFromInt")

        bigToInt = mkBase((arg) => {
            assertIsBigInt(arg);
            const v: bigint = arg.val.value;
            if (v > MAX_SAFE || v < MIN_SAFE) {
                return this.runtime.ret(
                    this.bigErr("bigint out of exact double range: " + v.toString(), lub(arg.lev, this.runtime.$t.pc)));
            }
            return this.runtime.ret(this.bigOk(Number(v), lub(arg.lev, this.runtime.$t.pc)));
        }, "bigToInt")

        bigToString = mkBase((arg) => {
            assertIsBigInt(arg);
            return this.runtime.ret(new LVal(arg.val.value.toString(), lub(arg.lev, this.runtime.$t.pc)));
        }, "bigToString")

        bigNeg = mkBase((arg) => {
            assertIsBigInt(arg);
            const v: bigint = arg.val.value;
            return this.runtime.ret(
                new LVal(new TroupeBigInt(-v), lub(arg.lev, this.runtime.$t.pc)));
        }, "bigNeg")

        bigAdd = this.bigBinOp("bigAdd", (a, b) => new TroupeBigInt(a + b))
        bigSub = this.bigBinOp("bigSub", (a, b) => new TroupeBigInt(a - b))
        bigMul = this.bigBinOp("bigMul", (a, b) => new TroupeBigInt(a * b))

        bigDiv = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsBigInt(arg.val[0]);
            assertIsBigInt(arg.val[1]);
            const b: bigint = arg.val[1].val.value;
            if (b === 0n) {
                this.runtime.$t.threadError("bigDiv: division by zero");
            }
            const a: bigint = arg.val[0].val.value;
            const lev = lub(arg.lev, arg.val[0].lev, arg.val[1].lev, this.runtime.$t.pc);
            return this.runtime.ret(new LVal(new TroupeBigInt(a / b), lev));
        }, "bigDiv")

        bigMod = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsBigInt(arg.val[0]);
            assertIsBigInt(arg.val[1]);
            const b: bigint = arg.val[1].val.value;
            if (b === 0n) {
                this.runtime.$t.threadError("bigMod: modulo by zero");
            }
            const a: bigint = arg.val[0].val.value;
            const lev = lub(arg.lev, arg.val[0].lev, arg.val[1].lev, this.runtime.$t.pc);
            return this.runtime.ret(new LVal(new TroupeBigInt(a % b), lev));
        }, "bigMod")

        bigCmp = this.bigBinOp("bigCmp", (a, b) => a < b ? -1 : (a > b ? 1 : 0))
    }
}
