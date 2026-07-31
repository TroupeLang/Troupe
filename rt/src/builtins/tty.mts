import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { Record } from '../Record.mjs';
import { lub, Level } from '../Level.mjs';
import { assertIsLocalObject, assertNormalState } from '../Asserts.mjs'
import { stdio_level } from './stdio.mjs';

/**
 * Terminal query primitives.
 *
 * Three pure queries over the stdio channel: whether a stream is a terminal,
 * what its dimensions are, and what level the channel runs at. None of them
 * changes any state — no listeners, no termios, no interaction with readline —
 * so they behave identically under both stdio models: an observation is not an
 * effect, and there is nothing for the sink check to gate.
 *
 * Labelling follows the channel model
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §1.1, §5.1):
 *
 *  - `ttyIsTTY` and `ttySize` return observations drawn *through* the channel,
 *    so their results carry the channel level, exactly as `freadln` labels a
 *    line with `mkValWithLev(s, stdio_level)` (stdio.mts).
 *  - `ttyLevel` returns run configuration — which level the operator started
 *    the runtime with — not anything the channel carried, so it returns at the
 *    caller's pc. This is the deliberate contrast with `levelOf`, which labels
 *    a returned level at `lub(pc, l)` because a value's level is provenance
 *    about that value (levelops.mts).
 *
 * Error disposition follows the SimpleFileIO rule: a bad argument is fatal, a
 * bad environment is a value. A non-descriptor or the wrong descriptor kills
 * the thread; a stream that is a pipe rather than a terminal is an `Err`
 * record, because `process.stdout.columns` is *undefined* on a pipe and an
 * unguarded read would surface as an internal runtime crash.
 *
 * Helper names carry a prefix: every builtin mixin contributes to one prototype
 * chain, so a plain `mkOk`/`mkErr` here would be shadowed by the identically
 * named helper of a mixin applied later (simplefileio.mts has both).
 */
export function BuiltinTty<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        /** The channel level joined with the pc: the level of an observation. */
        private ttyObservationLevel(): Level {
            return lub(this.runtime.$t.pc, stdio_level);
        }

        private mkTtyOk(value: LVal): LVal {
            const rec = Record.mkRecord([
                ['tag', new LVal('Ok', this.ttyObservationLevel())],
                ['value', value],
            ]);
            return this.runtime.$t.mkValWithLev(rec, stdio_level);
        }

        private mkTtyErr(reason: string): LVal {
            const lev = this.ttyObservationLevel();
            const errRec = Record.mkRecord([
                ['reason', new LVal(reason, lev)],
            ]);
            const rec = Record.mkRecord([
                ['tag', new LVal('Err', lev)],
                ['error', new LVal(errRec, lev)],
            ]);
            return this.runtime.$t.mkValWithLev(rec, stdio_level);
        }

        /**
         * Type-assert the argument as a descriptor and check it names one of
         * the admissible streams. The wrong descriptor is a programming error
         * and kills the thread, as it does for `freadln`/`fwrite`.
         */
        private ttyDescriptor(arg, admissible: any[], expected: string) {
            assertIsLocalObject(arg);
            const fd = arg.val._value;
            if (!admissible.includes(fd)) {
                this.runtime.$t
                    .threadError(`value ${arg.val.stringRep()} is not ${expected}`);
            }
            return fd;
        }

        ttyIsTTY = mkBase((arg) => {
            assertNormalState("ttyIsTTY")
            const fd = this.ttyDescriptor(
                arg, [process.stdin, process.stdout, process.stderr],
                "a standard descriptor");
            return this.runtime.ret(
                this.runtime.$t.mkValWithLev(fd.isTTY === true, stdio_level));
        }, "ttyIsTTY");

        ttySize = mkBase((arg) => {
            assertNormalState("ttySize")
            const fd = this.ttyDescriptor(
                arg, [process.stdout, process.stderr],
                "an output descriptor");

            if (fd.isTTY !== true) {
                return this.runtime.ret(this.mkTtyErr("not a terminal"));
            }
            const cols = fd.columns;
            const rows = fd.rows;
            if (typeof cols !== 'number' || typeof rows !== 'number') {
                return this.runtime.ret(this.mkTtyErr("terminal size unavailable"));
            }
            const lev = this.ttyObservationLevel();
            const size = Record.mkRecord([
                ['cols', new LVal(cols, lev)],
                ['rows', new LVal(rows, lev)],
            ]);
            return this.runtime.ret(this.mkTtyOk(new LVal(size, lev)));
        }, "ttySize");

        ttyLevel = mkBase((arg) => {
            assertNormalState("ttyLevel")
            this.ttyDescriptor(
                arg, [process.stdin, process.stdout, process.stderr],
                "a standard descriptor");
            // Run configuration, not channel content: the result is at the pc.
            return this.runtime.ret(this.runtime.$t.mkVal(stdio_level));
        }, "ttyLevel");
    }
}
