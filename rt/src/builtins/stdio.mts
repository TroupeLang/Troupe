import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LocalObject } from '../LocalObject.mjs'
import { levelFromFlag, flowsTo, actsFor, lub, ROOT, Level } from '../Level.mjs'
import { mkLevel } from '../Level.mjs'
import { assertIsAuthority, assertIsRootAuthority, assertIsLevel, assertIsNTuple, assertIsLocalObject, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { ErrorKind } from '../TroupeError.mjs';
import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
const argv = getCliArgs();

import * as rl from 'node:readline';

/**
 * The level the runtime was started at, from `--stdiolev` or the default.
 *
 * This is the ceiling: every level a program later names — the channel level it
 * sets, the level it reads a line at — must be one the startup level acts for.
 * The ceiling itself never moves, so lowering the channel level does not lower
 * what a later call may ask for.
 */
const stdio_ceiling = argv[TroupeCliArg.Stdiolev]
    ? levelFromFlag (TroupeCliArg.Stdiolev, argv[TroupeCliArg.Stdiolev])
    : ROOT

/**
 * The channel level in force, which `setStdioLevel` moves and every check on
 * the channel reads. It starts at the ceiling and stays at or below it.
 */
export let stdio_level = stdio_ceiling

/**
 * The check both level-taking primitives share: full authority admits a level
 * change, and the startup level bounds which level may be named.
 *
 * The bound is `actsFor`, as on `attenuate` (attenuate.mts): a level the
 * startup level acts for is weaker in both dimensions, so no call widens the
 * channel beyond what the operator granted on the command line.
 */
function checkLevelAuthority($t, operation: string, auth, lev) {
    assertIsAuthority(auth);
    assertIsRootAuthority(auth);
    assertIsLevel(lev);
    if (!actsFor(stdio_ceiling, lev.val)) {
        $t.threadError(
            `${operation} above the level the runtime started at\n` +
            ` | requested level ${lev.val.stringRep()}\n` +
            ` | startup level   ${stdio_ceiling.stringRep()}`,
            false, null, ErrorKind.IFCCheck);
    }
}

/** Buffer of input lines that have been provided but not consumed. */
const lineBuffer = [];

/** Callbacks for awakening Troupe threads currently blocked due to them waiting for inputs. */
const readlineCallbacks = []

/** For every new line, update either the buffer or notify a thread. */
function lineListener(input) {
    if (readlineCallbacks.length === 0) {
        lineBuffer.push(input);
    } else {
        const cb = readlineCallbacks.shift();
        cb(input);
    }
}

/** Node's readline interface. Created lazily on first use: on a terminal,
 * creating the interface puts stdin into raw mode with readline's own echo,
 * so a program that never reads a line must never trigger it. */
let readline: rl.Interface | null = null

function getReadline(): rl.Interface {
    if (readline === null) {
        readline = rl.createInterface({
            input: process.stdin,
            output: process.stdout
        })
        readline.on('line', lineListener)
    }
    return readline
}

export function closeReadline() {
    if (readline !== null) {
        readline.close()
        readline = null
    }
}

/**
 * Release the terminal from readline before another component takes it over.
 *
 * This closes the interface rather than pausing it: `pause()` leaves stdin in
 * readline's raw mode and merely stops delivering, whereas `close()` resets the
 * mode and releases the stream, which is the base state `ttyRawMode` then sets
 * raw mode from. `close()` is irreversible for an interface object, which is
 * why the interface is created lazily and can be re-created: a later `freadln`
 * builds a fresh one.
 */
export function suspendReadline() {
    closeReadline()
}

/**
 * The sink check on the stdio channel: an effect on it is
 * admitted only when everything it may reveal flows to the channel level.
 *
 * The shape is send's (send.mts): the pc is raised to the blocking label
 * first, because performing the effect discloses everything the thread has
 * observed so far; the operand levels then join the pc for the check.
 */
export function checkChannelEffect($t, operation: string, ...operandLevels: Level[]) {
    $t.raiseCurrentThreadPCToBlockingLev();
    const effectLevel = lub($t.pc, ...operandLevels);
    if (!flowsTo(effectLevel, stdio_level)) {
        $t.threadError(
            `${operation} above the stdio channel level\n` +
            ` | effect level  ${effectLevel.stringRep()}\n` +
            ` | pc level      ${$t.pc.stringRep()}\n` +
            ` | channel level ${stdio_level.stringRep()}`,
            false, null, ErrorKind.IFCCheck);
    }
}

export function BuiltinStdIo<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        /*
         * The write path of the ambient names.
         *
         * `print` and its neighbours are builtins, available to a program, a
         * module and a library alike. A descriptor names a stream and grants
         * nothing, so they acquire without an authority and the channel check
         * sits on the write, exactly as in `fwrite`. The helpers below are
         * what they are built from.
         */

        /** The descriptor an output operation names, or a thread error. */
        outputDescriptor(descr) {
            assertIsLocalObject(descr);
            const fd = descr.val._value;
            if (fd !== process.stdout && fd !== process.stderr) {
                this.runtime.$t
                    .threadError(`value ${descr.val.stringRep()} is not an output descriptor`);
            }
            return fd;
        }

        /** A value's printed form, and the level that printing it accumulated. */
        printedForm(arg, omitLabels: boolean): { text: string, lev: Level } {
            const taintRef = { lev: this.runtime.$t.pc };
            const text = this.runtime.$t.mkCopy(arg).stringRep(omitLabels, taintRef);
            return { text: text, lev: taintRef.lev };
        }

        /** One line onto an output descriptor, checked against the channel. */
        writeLine(fd, text: string, ...operandLevels: Level[]) {
            checkChannelEffect(this.runtime.$t,
                               fd === process.stderr ? "write to stderr" : "write to stdout",
                               ...operandLevels)
            fd.write(text + "\n");
            return this.runtime.ret(__unit);
        }

        /*
         * The three standard descriptors, as values rather than as something
         * acquired. A descriptor names a stream and grants nothing: it is the
         * operations on the stream that are checked against the channel level,
         * so there is nothing for an acquisition to show and nothing to check.
         * Generated code reaches a base name as a raw value, which is what
         * lets these be written `fwrite (stdout, s)` with no call.
         */
        stdin = new LocalObject(process.stdin);
        stdout = new LocalObject(process.stdout);
        stderr = new LocalObject(process.stderr);

        /** The descriptor an input operation names, or a thread error. */
        inputDescriptor(descr) {
            assertIsLocalObject(descr);
            const fd = descr.val._value;
            if (fd !== process.stdin) {
                this.runtime.$t
                    .threadError(`value ${descr.val.stringRep()} is not an input descriptor`);
            }
            return fd;
        }

        freadln = mkBase((arg) => {
            assertNormalState("freadLine")
            this.inputDescriptor(arg);
            return this.readChannelLine(stdio_level, arg.lev);
        }, "freadln");

        freadlnAtLevel = mkBase((arg) => {
            assertNormalState("freadlnAtLevel")
            assertIsNTuple(arg, 3);
            this.inputDescriptor(arg.val[0]);
            checkLevelAuthority(this.runtime.$t, "read from stdin at a level",
                                arg.val[1], arg.val[2]);
            return this.readChannelLine(arg.val[2].val,
                                        arg.lev, arg.val[0].lev,
                                        arg.val[1].lev, arg.val[2].lev);
        }, "freadlnAtLevel");

        /**
         * The channel level, for the rest of the run and for every thread.
         *
         * A level change is an effect on the channel — `ttyLevel` reports it
         * and a later write is checked against it — so it carries the sink
         * check that a write carries, on top of the authority and the ceiling.
         */
        setStdioLevel = mkBase((arg) => {
            assertNormalState("setStdioLevel")
            assertIsNTuple(arg, 2);
            checkLevelAuthority(this.runtime.$t, "stdio channel level",
                                arg.val[0], arg.val[1]);
            checkChannelEffect(this.runtime.$t, "level change",
                               arg.lev, arg.val[0].lev, arg.val[1].lev);
            stdio_level = arg.val[1].val;
            return this.runtime.ret(__unit);
        }, "setStdioLevel");

        /**
         * One line off stdin, blocking until it arrives, at `readLevel`: the
         * line is labelled at that level and the reading thread's blocking
         * label rises to it.
         *
         * `freadln` reads at the channel level. `freadlnAtLevel` reads at the
         * level its caller names, which is a declassification of the input and
         * is what the full authority there pays for; the consume below is
         * checked against the channel level either way.
         *
         * The level is taken as an argument rather than read from
         * `stdio_level` inside the delivery callback: `setStdioLevel` may move
         * the channel level while a reader is blocked, and the line a reader
         * waited for is the one the channel was carrying when it asked.
         */
        readChannelLine(readLevel: Level, ...operandLevels: Level[]) {
            // Consuming a line is an observable effect on the channel: a later
            // reader no longer sees it, so it carries the same sink check as a
            // write.
            checkChannelEffect(this.runtime.$t, "read from stdin", ...operandLevels)

            getReadline()

            this.runtime.$t.raiseBlockingThreadLev(readLevel)

            // If input already has been provided, then proceed immediately.
            if (lineBuffer.length > 0) {
                let s = lineBuffer.shift();
                let r = this.runtime.$t.mkValWithLev(s, readLevel);
                return this.runtime.$t.returnImmediateLValue(r);
            }

            // Otherwise, wait for input to arrive.
            readlineCallbacks.push((s) => {
                let r = this.runtime.$t.mkValWithLev(s, readLevel)
                this.runtime.$t.returnSuspended(r)
                this.runtime.__sched.scheduleThread(this.runtime.$t);
                this.runtime.__sched.resumeLoopAsync()
            });
        }

        fwrite = mkBase((arg) => {
            assertNormalState("fwrite")
            assertIsNTuple(arg, 2);

            assertIsLocalObject(arg.val[0]);

            const fd = arg.val[0].val._value;
            if (fd !== process.stdout && fd !== process.stderr) {
                this.runtime.$t
                    .threadError(`value ${arg.val[0].val.stringRep()} is not an output descriptor`);
            }

            assertIsString(arg.val[1]);

            // Bytes appearing on the channel are an effect on it.
            checkChannelEffect(this.runtime.$t,
                               fd === process.stderr ? "write to stderr" : "write to stdout",
                               arg.lev, arg.val[0].lev, arg.val[1].lev)

            fd.write(arg.val[1].val);
            return this.runtime.ret(__unit);
        }, "fwrite");

        // The ambient names: what `print` and its neighbours resolve to,
        // wherever they are written.

        fwriteln = mkBase((arg) => {
            assertNormalState("fwriteln")
            assertIsNTuple(arg, 2);
            const fd = this.outputDescriptor(arg.val[0]);
            assertIsString(arg.val[1]);
            return this.writeLine(fd, arg.val[1].val,
                                  arg.lev, arg.val[0].lev, arg.val[1].lev);
        }, "fwriteln");

        fwritelnWithLabels = mkBase((arg) => {
            assertNormalState("fwritelnWithLabels")
            assertIsNTuple(arg, 2);
            const fd = this.outputDescriptor(arg.val[0]);
            const printed = this.printedForm(arg.val[1], false);
            return this.writeLine(fd, printed.text,
                                  arg.lev, arg.val[0].lev, printed.lev);
        }, "fwritelnWithLabels");

        printString = mkBase((arg) => {
            assertNormalState("printString")
            assertIsString(arg);
            return this.writeLine(process.stdout, arg.val, arg.lev);
        }, "printString");

        print = mkBase((arg) => {
            assertNormalState("print")
            const printed = this.printedForm(arg, true);
            return this.writeLine(process.stdout, printed.text, arg.lev, printed.lev);
        }, "print");

        printWithLabels = mkBase((arg) => {
            assertNormalState("printWithLabels")
            const printed = this.printedForm(arg, false);
            return this.writeLine(process.stdout, printed.text, arg.lev, printed.lev);
        }, "printWithLabels");

        inputLine = mkBase((arg) => {
            assertNormalState("inputLine")
            // The wrapper it replaces ignores its argument, so this does too.
            return this.readChannelLine(stdio_level, arg.lev);
        }, "inputLine");
    }
}
