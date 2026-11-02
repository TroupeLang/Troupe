'use strict'
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { ROOT } from '../../Level.mjs';
import { assertIsNTuple, assertIsRootAuthority, assertIsString } from '../../Asserts.mjs'
import { getRuntimeObject } from '../../SysState.mjs'

function errMessage(e: unknown): string {
    return e instanceof Error ? e.message : String(e);
}

function mkOk(value: LVal): LVal {
    const rec = Record.mkRecord([
        ['tag', new LVal('Ok', ROOT)],
        ['value', value],
    ]);
    return new LVal(rec, ROOT);
}

function mkErr(reason: string, url: string): LVal {
    const errRec = Record.mkRecord([
        ['reason', new LVal(reason, ROOT)],
        ['url', new LVal(url, ROOT)],
    ]);
    const rec = Record.mkRecord([
        ['tag', new LVal('Err', ROOT)],
        ['error', new LVal(errRec, ROOT)],
    ]);
    return new LVal(rec, ROOT);
}

function suspendWithResult(build: () => Promise<LVal>) {
    const runtime = getRuntimeObject();
    const theThread = runtime.$t;
    (async () => {
        let res: LVal;
        try {
            res = await build();
        } catch (e) {
            theThread.throwInSuspended('Http internal error: ' + errMessage(e));
            runtime.__sched.scheduleThread(theThread);
            runtime.__sched.resumeLoopAsync();
            return;
        }
        theThread.returnSuspended(res);
        runtime.__sched.scheduleThread(theThread);
        runtime.__sched.resumeLoopAsync();
    })();
}

function doPost(url: string, body: string, bearer: string | null) {
    suspendWithResult(async () => {
        const headers: { [k: string]: string } = { 'content-type': 'application/json' };
        if (bearer !== null) headers['authorization'] = 'Bearer ' + bearer;
        let response: Response;
        try {
            response = await fetch(url, { method: 'POST', headers, body });
        } catch (e) {
            return mkErr(errMessage(e), url);
        }
        let text: string;
        try {
            text = await response.text();
        } catch (e) {
            return mkErr(errMessage(e), url);
        }
        if (!response.ok) {
            return mkErr(`http ${response.status}: ${text}`, url);
        }
        return mkOk(new LVal(text, ROOT));
    });
}

export const httpExports = {
    post: mkBase((larg) => {
        assertIsNTuple(larg, 3);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        assertIsString(arg[2]);
        doPost(arg[1].val, arg[2].val, null);
    }, 'post'),

    postBearer: mkBase((larg) => {
        assertIsNTuple(larg, 4);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        assertIsString(arg[2]);
        assertIsString(arg[3]);
        doPost(arg[1].val, arg[2].val, arg[3].val);
    }, 'postBearer'),
}
