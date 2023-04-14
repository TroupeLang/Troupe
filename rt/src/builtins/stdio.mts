import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LocalObject } from '../LocalObject.mjs'
import * as options from '../options.mjs'
import { mkLevel } from '../options.mjs'
import { assertIsAuthority, assertIsTopAuthority, assertIsNTuple, assertIsLocalObject, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import yargs from 'yargs'

const levels = options;
const flowsTo = levels.flowsTo;

import * as _rl from 'node:readline';


const readline = _rl.createInterface({
    input: process.stdin,
    output: process.stdout
})

const lineBuffer = [];
const readlineCallbacks = []

const __stdio_lev = yargs.argv.stdiolev ? mkLevel (yargs.argv.stdiolev): levels.TOP

function lineListener(input) {
    if (readlineCallbacks.length > 0) {
        let cb = readlineCallbacks.shift();
        cb(input);
    } else {
        lineBuffer.push(input);
    }
}

readline.on('line', lineListener)

export function closeReadline() {
    readline.close()
}

export function BuiltinStdIo<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        getStdout = mkBase((arg) => {
            assertIsAuthority(arg)
            let sufficentAuthority = flowsTo(__stdio_lev, arg.val.authorityLevel)
            if (sufficentAuthority) {
                let obj = new LocalObject(process.stdout);
                return this.runtime.ret(this.mkVal(obj))
            } else {
                this.runtime.$t.threadError
                     (`Not sufficient authority in getStdout\n` + 
                     ` | Provided authority level ${arg.val.authorityLevel.stringRep()}\n` +
                     ` | Required authority level ${__stdio_lev.stringRep()}`)
            }
            
        })

        fprintln = mkBase((arg) => {
            assertNormalState("fprintln")
            assertIsNTuple(arg, 2);
            assertIsLocalObject(arg.val[0]);

            let out = arg.val[0].val._value;
            out.write(arg.val[1].stringRep(true) + "\n");
            return this.runtime.ret(__unit);
        });

        fprintlnWithLabels = mkBase((arg) => {
            assertNormalState("fprintlnWithLabels")
            assertIsNTuple(arg, 2);
            assertIsLocalObject(arg.val[0]);
            let out = arg.val[0].val._value;
            out.write(this.runtime.$t.mkCopy(arg.val[1]).stringRep(false) + "\n");
            // out.write((arg.val[1]).stringRep(false) + "\n");
            return this.runtime.ret(__unit);
        });

        fwrite = mkBase((arg) => {
            assertNormalState("fwrite")
            assertIsNTuple(arg, 2);
            assertIsLocalObject(arg.val[0]);
            assertIsString(arg.val[1]);
            let out = arg.val[0].val._value;
            out.write(arg.val[1].val);
            return this.runtime.ret(__unit);
        }, "fwrite");

        inputLine = mkBase((arg) => {
            assertNormalState("inputLine")
            assertIsUnit(arg)
            let theThread = this.runtime.$t;
            theThread.raiseBlockingThreadLev(levels.TOP)
            if (lineBuffer.length > 0) {
                let s = lineBuffer.shift();
                let r = theThread.mkValWithLev(s, levels.TOP);
                return theThread.returnImmediateLValue(r);
            } else {
                readlineCallbacks.push((s) => {
                    let r = theThread.mkValWithLev(s, levels.TOP)
                    theThread.returnSuspended(r)
                    this.runtime.__sched.scheduleThread(theThread);
                    this.runtime.__sched.resumeLoopAsync()
                })  
            }
        }, "inputLine")

        rt_question = mkBase((arg) => {
            assertNormalState("rt_question")
            readline.removeListener('line', lineListener);
            let theThread = this.runtime.$t;
            assertIsString(arg);
            theThread.raiseBlockingThreadLev(levels.TOP)
            readline.question(arg.val, (s) => {
                let r = theThread.mkValWithLev(s, levels.TOP)
                theThread.returnSuspended(r)
                this.runtime.__sched.scheduleThread(theThread);
                this.runtime.__sched.resumeLoopAsync()
                readline.on('line', lineListener)
            })

        }, "question")
    }
}