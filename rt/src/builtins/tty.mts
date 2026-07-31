import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { Record } from '../Record.mjs';
import { lub, Level } from '../Level.mjs';
import { assertIsBoolean, assertIsLocalObject, assertIsNTuple, assertNormalState } from '../Asserts.mjs'
import { __unitbase } from '../UnitBase.mjs';
import { stdio_level, IFC_MODEL, checkChannelEffect, suspendReadline } from './stdio.mjs';

/**
 * Terminal primitives.
 *
 * Three pure queries over the stdio channel — whether a stream is a terminal,
 * what its dimensions are, and what level the channel runs at — and one
 * operation that changes the terminal's line discipline. The queries change no
 * state: no listeners, no termios, no interaction with readline, so they behave
 * identically under both stdio models, an observation being no effect and
 * nothing for the sink check to gate. `ttyRawMode` is an effect on the channel
 * and carries the sink check under the IFC model, as `fwrite` does.
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
 * record, because `process.stdout.columns` and `process.stdin.setRawMode` are
 * *undefined* on a pipe and an unguarded read or call would surface as an
 * internal runtime crash. `setRawMode` throwing — EIO on a terminal that has
 * gone away — is the same class and is also an `Err`.
 *
 * Helper names carry a prefix: every builtin mixin contributes to one prototype
 * chain, so a plain `mkOk`/`mkErr` here would be shadowed by the identically
 * named helper of a mixin applied later (simplefileio.mts has both).
 */
/**
 * Whether *this* component put stdin into raw mode. A program that read a line
 * on a terminal is in readline's raw mode instead, and readline's own `close()`
 * resets that; the restore hook must not reach past its own doing.
 */
let weSetRawMode = false;

/**
 * Reset the terminal state the runtime is responsible for. Called from
 * `cleanupAsync` (runtimeMonitored.mts), which runs for every program, on every
 * termination route that reaches it — so this must be safe when nothing was set
 * up, and safe to run twice.
 *
 * It runs outside any thread: no labels, no `$t`, no scheduler.
 *
 * It restores *termios*, not screen state. The alternate screen buffer, cursor
 * visibility and cursor shape are escapes a program emitted, and unwinding them
 * is the program's business
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §4.4).
 */
export function ttyRestore() {
    try {
        if (weSetRawMode && process.stdin.isTTY) {
            process.stdin.setRawMode(false);
        }
    } catch (e) {
        // A terminal that has gone away throws here; cleanup must not fail.
    }
    weSetRawMode = false;
    try {
        // Releases the event-loop reference the stream may hold. Separate from
        // the reset above so that a throwing reset does not skip it.
        process.stdin.pause();
    } catch (e) {
        // Same: nothing this hook does may fail cleanup.
    }
}

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

        /**
         * Put the terminal into or out of raw mode.
         *
         * Raw mode changes the echo and the line discipline of the channel
         * visibly, so under the IFC model it carries the same sink check as
         * `fwrite`; under the capability model it is unchecked, the descriptor
         * having been the gate.
         *
         * Entering raw mode first releases readline: on a terminal the
         * interface owns stdin's mode and its own echo, so raw mode has to be
         * set from a base state readline is not holding. A later `freadln`
         * re-creates the interface.
         */
        ttyRawMode = mkBase((arg) => {
            assertNormalState("ttyRawMode")
            assertIsNTuple(arg, 2);
            const fd = this.ttyDescriptor(
                arg.val[0], [process.stdin], "an input descriptor");
            assertIsBoolean(arg.val[1]);
            const on = arg.val[1].val === true;

            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t, "terminal raw-mode change",
                                   arg.lev, arg.val[0].lev, arg.val[1].lev)
            }

            if (fd.isTTY !== true || typeof fd.setRawMode !== 'function') {
                return this.runtime.ret(this.mkTtyErr("not a terminal"));
            }
            try {
                if (on) {
                    suspendReadline();
                    fd.setRawMode(true);
                    weSetRawMode = true;
                } else {
                    fd.setRawMode(false);
                    weSetRawMode = false;
                }
            } catch (e) {
                return this.runtime.ret(
                    this.mkTtyErr(e instanceof Error ? e.message : String(e)));
            }
            return this.runtime.ret(
                this.mkTtyOk(new LVal(__unitbase, this.ttyObservationLevel())));
        }, "ttyRawMode");
    }
}
