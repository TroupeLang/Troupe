import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LocalObject } from '../LocalObject.mjs'
import { mkV1Level, flowsTo, ROOT } from '../Level.mjs'
import { mkLevel } from '../Level.mjs'
import { assertIsAuthority, assertIsRootAuthority, assertIsNTuple, assertIsLocalObject, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
const argv = getCliArgs();

import * as rl from 'node:readline';

const stdio_level = argv[TroupeCliArg.Stdiolev]
    ? mkV1Level (argv[TroupeCliArg.Stdiolev])
    : ROOT

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

export function BuiltinStdIo<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        stdin = mkBase((arg) => {
            assertIsAuthority(arg)

            const sufficentAuthority = flowsTo(stdio_level, arg.val.authorityLevel)
            if (!sufficentAuthority) {
                this.runtime.$t.threadError
                (`Not sufficient authority for stdIn\n` +
                    ` | Provided authority level ${arg.val.authorityLevel.stringRep()}\n` +
                    ` | Required authority level ${stdio_level.stringRep()}`);
                return;
            }

            return this.runtime.ret(this.mkVal(new LocalObject(process.stdin)))
        }, "stdin");

        stdout = mkBase((arg) => {
            assertIsAuthority(arg)

            const sufficentAuthority = flowsTo(stdio_level, arg.val.authorityLevel)
            if (!sufficentAuthority) {
                this.runtime.$t.threadError
                     (`Not sufficient authority for stdOut\n` +
                     ` | Provided authority level ${arg.val.authorityLevel.stringRep()}\n` +
                     ` | Required authority level ${stdio_level.stringRep()}`)
                return;
            }

            return this.runtime.ret(this.mkVal(new LocalObject(process.stdout)));
        }, "stdout");

        stderr = mkBase((arg) => {
            assertIsAuthority(arg)

            const sufficentAuthority = flowsTo(stdio_level, arg.val.authorityLevel)
            if (!sufficentAuthority) {
                this.runtime.$t.threadError
                (`Not sufficient authority for stdErr\n` +
                    ` | Provided authority level ${arg.val.authorityLevel.stringRep()}\n` +
                    ` | Required authority level ${stdio_level.stringRep()}`)
                return;
            }

            return this.runtime.ret(this.mkVal(new LocalObject(process.stderr)));
        }, "stderr");

        freadln = mkBase((arg) => {
            assertNormalState("freadLine")

            assertIsLocalObject(arg);
            const fd = arg.val._value;
            if (fd !== process.stdin) {
                this.runtime.$t
                    .threadError(`value ${fd.stringRep()} is not an input descriptor`);
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
        }, "freadln");

        fwrite = mkBase((arg) => {
            assertNormalState("fwrite")
            assertIsNTuple(arg, 2);

            assertIsLocalObject(arg.val[0]);

            const fd = arg.val[0].val._value;
            if (fd !== process.stdout && fd !== process.stderr) {
                this.runtime.$t
                    .threadError(`value ${fd.stringRep()} is not an output descriptor`);
            }

            assertIsString(arg.val[1]);

            fd.write(arg.val[1].val);
            return this.runtime.ret(__unit);
        }, "fwrite");
    }
}
