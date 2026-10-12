'use strict'
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { lub, flowsTo, BOT, Level } from '../../Level.mjs';
import { assertIsAuthority, assertIsLevel, assertIsNTuple, assertIsNumber, assertIsLocalProcessId, assertIsRecord, rawAssertRecordHasField, assertIsRootAuthority, assertIsString, assertIsUnit, assertNormalState } from '../../Asserts.mjs'
import { __unitbase } from '../../UnitBase.mjs';
import { mkList, mkTuple } from '../../ValuesUtil.mjs';
import { RuntimeInterface } from '../../RuntimeInterface.mjs';
import { getRuntimeObject } from '../../SysState.mjs';
import { ErrorKind } from '../../TroupeError.mjs';

/**
 * TestQuestions — a test fixture (manifest ffi/TestQuestions.exports, library
 * lib/TestQuestions.trp) for host questions delivered as messages: a Node
 * timer stands where a libp2p hook would, puts `(name, arg, reply)` into the
 * mailbox of the process set by `setProcess`, and awaits the promise that
 * `reply` resolves. `report` returns what each question came to.
 */

type Outcome = 'pending' | 'answered' | 'rejected' | 'noProcess';

interface QuestionRecord {
    name: string;
    outcome: Outcome;
    value: any;
    askedAt: number;
    timerLate: number;
    ms: number | null;
    lateCalls: number;
    effect: string;
}

const DENY = true;

let questionProcess: LVal | null = null;
let questionRuntime: RuntimeInterface | null = null;
let questionPc: Level = BOT;

let records: QuestionRecord[] = [];

function askNow(name: string, argVal: any, argLevel: Level, dueAt: number) {
    const askedAt = performance.now();
    const rec: QuestionRecord = {
        name, outcome: 'pending', value: null,
        askedAt, timerLate: askedAt - dueAt, ms: null, lateCalls: 0, effect: ''
    };
    records.push(rec);

    let resolvePromise: (v: boolean) => void;
    const answer = new Promise<boolean>((res) => { resolvePromise = res; });
    const resolve = (v: boolean, outcome: Outcome) => {
        rec.outcome = outcome;
        rec.value = v;
        resolvePromise(v);
    };

    const reply = mkBase((arg) => {
        const $t = getRuntimeObject().$t;
        if (rec.outcome !== 'pending') {
            rec.lateCalls++;
            return getRuntimeObject().ret($t.mkVal(__unitbase));
        }
        // The check of checkChannelEffect (builtins/stdio.mts) against BOT.
        // A failure resolves the question with a deny before the thread error,
        // so the asking side never waits on a caller that has died.
        $t.raiseCurrentThreadPCToBlockingLev();
        const effectLevel = lub($t.pc, arg.lev);
        if (!flowsTo(effectLevel, BOT)) {
            rec.effect = effectLevel.stringRep();
            resolve(DENY, 'rejected');
            $t.threadError(
                `reply above the answer's channel level\n` +
                ` | effect level  ${effectLevel.stringRep()}\n` +
                ` | pc level      ${$t.pc.stringRep()}\n` +
                ` | channel level ${BOT.stringRep()}`,
                false, null, ErrorKind.IFCCheck);
        }
        if (typeof arg.val !== 'boolean') {
            rec.effect = 'not a boolean';
            resolve(DENY, 'rejected');
            $t.threadError(`reply with ${arg.stringRep()}, which is not a boolean`);
        }
        resolve(arg.val, 'answered');
        return getRuntimeObject().ret($t.mkVal(__unitbase));
    }, "reply");

    (async () => {
        if (questionProcess === null || questionRuntime === null
            || !questionRuntime.__sched.isAlive(questionProcess)) {
            resolve(DENY, 'noProcess');
        } else {
            const msg = new LVal(mkTuple([
                new LVal(name, questionPc),
                new LVal(argVal, lub(questionPc, argLevel)),
                new LVal(reply, questionPc),
            ]), questionPc);
            questionRuntime.__mbox.enqueueFromHost(questionProcess, msg, questionPc);
        }
        await answer;
        rec.ms = performance.now() - rec.askedAt;
    })();
}

function reportRecord(rec: QuestionRecord): LVal {
    const ms = rec.ms === null ? performance.now() - rec.askedAt : rec.ms;
    const at = (x: any) => new LVal(x, BOT);
    return at(Record.mkRecord([
        ['name', at(rec.name)],
        ['outcome', at(rec.outcome)],
        ['value', at(rec.value === null ? "none" : String(rec.value))],
        ['ms', at(Math.round(ms * 100) / 100)],
        ['timerLate', at(Math.round(rec.timerLate * 100) / 100)],
        ['lateCalls', at(rec.lateCalls)],
        ['effect', at(rec.effect)],
    ]));
}

export const testQuestionsExports = {
    setProcess: mkBase((arg) => {
        assertNormalState("setProcess")
        assertIsNTuple(arg, 2);
        assertIsAuthority(arg.val[0]);
        assertIsRootAuthority(arg.val[0]);
        assertIsLocalProcessId(arg.val[1], "questions");
        const $t = getRuntimeObject().$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev));
        questionProcess = arg.val[1];
        questionRuntime = getRuntimeObject();
        questionPc = $t.pc;
        return getRuntimeObject().ret($t.mkVal(__unitbase));
    }, "setProcess"),

    ask: mkBase((arg) => {
        assertNormalState("ask")
        assertIsRecord(arg);
        const field = (f: string) => {
            rawAssertRecordHasField(arg.val, f);
            return arg.val.getField(f);
        };
        const name = field('name'), argLevel = field('level'), delay = field('delay');
        assertIsString(name);
        assertIsLevel(argLevel);
        assertIsNumber(delay);
        const argVal = field('arg').val;
        const dueAt = performance.now() + delay.val;
        setTimeout(() => askNow(name.val, argVal, argLevel.val, dueAt), delay.val);
        return getRuntimeObject().ret(getRuntimeObject().$t.mkVal(__unitbase));
    }, "ask"),

    report: mkBase((arg) => {
        assertIsUnit(arg);
        return getRuntimeObject().ret(
            getRuntimeObject().$t.mkValWithLev(mkList(records.map(reportRecord)), BOT));
    }, "report"),

    clear: mkBase((arg) => {
        assertIsUnit(arg);
        records = [];
        return getRuntimeObject().ret(getRuntimeObject().$t.mkVal(__unitbase));
    }, "clear"),
}
