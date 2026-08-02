import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LocalObject } from '../LocalObject.mjs'
import { levelFromFlag, flowsTo, actsFor, lub, ROOT, Level } from '../Level.mjs'
import { mkLevel } from '../Level.mjs'
import { assertIsAuthority, assertIsRootAuthority, assertIsNTuple, assertIsLocalObject, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { ErrorKind } from '../TroupeError.mjs';
import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
const argv = getCliArgs();

import * as rl from 'node:readline';

export const stdio_level = argv[TroupeCliArg.Stdiolev]
    ? levelFromFlag (TroupeCliArg.Stdiolev, argv[TroupeCliArg.Stdiolev])
    : ROOT

/**
 * The stdio enforcement model (`--stdio-model`).
 *
 * Under `capability`, acquiring a descriptor is checked against the authority
 * shown and the operations are unchecked. Under `ifc`, stdio is a channel at
 * `stdio_level`: acquisition is unchecked, observations are labelled at the
 * channel level, and every effect on the channel is checked against it.
 */
export const IFC_MODEL = argv[TroupeCliArg.StdioModel] === 'ifc'

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
 * The sink check of the IFC stdio model: an effect on the stdio channel is
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
         * The write path of the IFC model's ambient names.
         *
         * A program compiled with `--stdio-model=ifc` gets `print` and its
         * neighbours from the builtins at the end of this class rather than
         * from the compiler-injected wrappers
         * (`compiler/src/AddAmbientMethods.hs`), which exist to thread the
         * program's authority into an acquisition. Under this model a
         * descriptor grants nothing, so they acquire without one and the
         * channel check sits on the write, exactly as in `fwrite`. The helpers
         * below are what they are built from.
         */

        /** Refuse a name that acquires implicitly when the model checks acquisition. */
        refuseUnderCapabilityModel(name: string, alternative: string) {
            if (!IFC_MODEL) {
                this.runtime.$t.threadError(
                    `${name} is unavailable under the capability stdio model\n` +
                    ` | this model's ${name} is the wrapper the compiler injects into a program, and\n` +
                    ` |   this call is outside one: module or library code, or a program\n` +
                    ` |   compiled with --stdio-model=ifc\n` +
                    ` | run with --stdio-model=ifc, or name a descriptor: ${alternative}`);
            }
        }

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
            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t,
                                   fd === process.stderr ? "write to stderr" : "write to stdout",
                                   ...operandLevels)
            }
            fd.write(text + "\n");
            return this.runtime.ret(__unit);
        }

        /**
         * The capability model's acquisition check: the authority shown must
         * suffice for the channel level, which is `actsFor(authority, level)`
         * — the relation the downgrade checks use. Under the IFC model
         * acquisition is unchecked: acquiring a descriptor observes nothing
         * and effects nothing, and enforcement sits on the operations.
         */
        checkAcquisition(arg, streamName: string) {
            if (IFC_MODEL) {
                return;
            }
            if (!actsFor(arg.val.authorityLevel, stdio_level)) {
                this.runtime.$t.threadError
                    (`Not sufficient authority for ${streamName}\n` +
                     ` | Provided authority level ${arg.val.authorityLevel.stringRep()}\n` +
                     ` | Required authority level ${stdio_level.stringRep()}`)
            }
        }

        stdin = mkBase((arg) => {
            assertIsAuthority(arg)
            this.checkAcquisition(arg, "stdIn")
            return this.runtime.ret(this.mkVal(new LocalObject(process.stdin)))
        }, "stdin");

        stdout = mkBase((arg) => {
            assertIsAuthority(arg)
            this.checkAcquisition(arg, "stdOut")
            return this.runtime.ret(this.mkVal(new LocalObject(process.stdout)));
        }, "stdout");

        stderr = mkBase((arg) => {
            assertIsAuthority(arg)
            this.checkAcquisition(arg, "stdErr")
            return this.runtime.ret(this.mkVal(new LocalObject(process.stderr)));
        }, "stderr");

        freadln = mkBase((arg) => {
            assertNormalState("freadLine")

            assertIsLocalObject(arg);
            const fd = arg.val._value;
            if (fd !== process.stdin) {
                this.runtime.$t
                    .threadError(`value ${arg.val.stringRep()} is not an input descriptor`);
            }

            return this.readChannelLine(arg.lev);
        }, "freadln");

        /** One line off stdin, at the channel level, blocking until it arrives. */
        readChannelLine(...operandLevels: Level[]) {
            // Consuming a line is an observable effect on the channel: a later
            // reader no longer sees it. Under the IFC model it carries the same
            // sink check as a write.
            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t, "read from stdin", ...operandLevels)
            }

            getReadline()

            this.runtime.$t.raiseBlockingThreadLev(stdio_level)

            // If input already has been provided, then proceed immediately.
            if (lineBuffer.length > 0) {
                let s = lineBuffer.shift();
                let r = this.runtime.$t.mkValWithLev(s, stdio_level);
                return this.runtime.$t.returnImmediateLValue(r);
            }

            // Otherwise, wait for input to arrive.
            readlineCallbacks.push((s) => {
                let r = this.runtime.$t.mkValWithLev(s, stdio_level)
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
            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t,
                                   fd === process.stderr ? "write to stderr" : "write to stdout",
                                   arg.lev, arg.val[0].lev, arg.val[1].lev)
            }

            fd.write(arg.val[1].val);
            return this.runtime.ret(__unit);
        }, "fwrite");

        // The ambient names. A program compiled with --stdio-model=capability
        // shadows all six with the wrappers the compiler injects; these are
        // what a program compiled with --stdio-model=ifc — and any library,
        // which never had the wrappers — resolves them to.

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
            this.refuseUnderCapabilityModel("printString", "fwrite (stdout authority, s)")
            assertIsString(arg);
            return this.writeLine(process.stdout, arg.val, arg.lev);
        }, "printString");

        print = mkBase((arg) => {
            assertNormalState("print")
            this.refuseUnderCapabilityModel("print", "fwrite (stdout authority, toString x)")
            const printed = this.printedForm(arg, true);
            return this.writeLine(process.stdout, printed.text, arg.lev, printed.lev);
        }, "print");

        printWithLabels = mkBase((arg) => {
            assertNormalState("printWithLabels")
            this.refuseUnderCapabilityModel("printWithLabels", "fwrite (stdout authority, toStringL x)")
            const printed = this.printedForm(arg, false);
            return this.writeLine(process.stdout, printed.text, arg.lev, printed.lev);
        }, "printWithLabels");

        inputLine = mkBase((arg) => {
            assertNormalState("inputLine")
            this.refuseUnderCapabilityModel("inputLine", "freadln (stdin authority)")
            // The wrapper it replaces ignores its argument, so this does too.
            return this.readChannelLine(arg.lev);
        }, "inputLine");
    }
}
