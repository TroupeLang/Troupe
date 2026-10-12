'use strict'
import type { PeerId } from '@libp2p/interface';
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { lub, flowsTo, BOT, Level } from '../../Level.mjs';
import { assertIsLocalProcessId, assertIsNTuple, assertIsRootAuthority, assertNormalState } from '../../Asserts.mjs'
import { __unitbase } from '../../UnitBase.mjs';
import { mkTuple } from '../../ValuesUtil.mjs';
import { getRuntimeObject } from '../../SysState.mjs';
import { ErrorKind } from '../../TroupeError.mjs';
import { nodeTrustLevel } from '../../TrustManager.mjs';
import { getCliArgs, TroupeCliArg } from '../../TroupeCliArgs.mjs';

/**
 * P2PRelayControl — the native module of a relay node (manifest
 * ffi/P2PRelayControl.exports, library lib/P2PRelayControl.trp).
 *
 * The libp2p relay service asks the node's connection gater whether to deny a
 * reservation and whether to deny a relayed connection. Each question becomes
 * the message `(name, arg, reply)` in the mailbox of the process set by
 * `setConnectionGater`; libp2p awaits the promise that `reply` resolves. With
 * no process set, or a dead one at delivery, the answer is deny.
 */

const DENY = true;

let gaterProcess: LVal | null = null;
let gaterPc: Level = BOT;

function isRelayNode(): boolean {
    return getCliArgs()[TroupeCliArg.StartRelay] !== undefined;
}

function ask(name: string, mkArg: (pc: Level) => LVal): Promise<boolean> {
    const rt = getRuntimeObject();
    if (gaterProcess === null || !rt.__sched.isAlive(gaterProcess)) {
        return Promise.resolve(DENY);
    }

    let resolveAnswer: (v: boolean) => void;
    const answer = new Promise<boolean>((res) => { resolveAnswer = res; });
    let answered = false;
    const resolve = (v: boolean) => {
        answered = true;
        resolveAnswer(v);
    };

    const reply = mkBase((arg) => {
        const $t = getRuntimeObject().$t;
        if (answered) {
            return getRuntimeObject().ret($t.mkVal(__unitbase));
        }
        // The check of checkChannelEffect (builtins/stdio.mts) against BOT. A
        // failure resolves the question with a deny before the thread error, so
        // libp2p never waits on a caller that has died.
        $t.raiseCurrentThreadPCToBlockingLev();
        const effectLevel = lub($t.pc, arg.lev);
        if (!flowsTo(effectLevel, BOT)) {
            resolve(DENY);
            $t.threadError(
                `reply above the answer's channel level\n` +
                ` | effect level  ${effectLevel.stringRep()}\n` +
                ` | pc level      ${$t.pc.stringRep()}\n` +
                ` | channel level ${BOT.stringRep()}`,
                false, null, ErrorKind.IFCCheck);
        }
        if (typeof arg.val !== 'boolean') {
            resolve(DENY);
            $t.threadError(`reply with ${arg.stringRep()}, which is not a boolean`);
        }
        resolve(arg.val);
        return getRuntimeObject().ret($t.mkVal(__unitbase));
    }, "reply");

    const pc = gaterPc;
    const msg = new LVal(mkTuple([
        new LVal(name, pc),
        mkArg(pc),
        new LVal(reply, pc),
    ]), pc);
    rt.__mbox.enqueueFromHost(gaterProcess, msg, pc);
    return answer;
}

function peerIdAt(peer: PeerId, pc: Level): LVal {
    const id = peer.toString();
    return new LVal(id, lub(pc, nodeTrustLevel(id)));
}

export function denyInboundRelayReservation(source: PeerId): Promise<boolean> {
    return ask("denyInboundRelayReservation", (pc) => peerIdAt(source, pc));
}

export function denyOutboundRelayedConnection(source: PeerId, destination: PeerId): Promise<boolean> {
    return ask("denyOutboundRelayedConnection", (pc) =>
        new LVal(mkTuple([peerIdAt(source, pc), peerIdAt(destination, pc)]), pc));
}

export const p2pRelayControlExports = {
    setConnectionGater: mkBase((arg) => {
        assertNormalState("setConnectionGater")
        assertIsNTuple(arg, 2);
        assertIsRootAuthority(arg.val[0]);
        assertIsLocalProcessId(arg.val[1], "connection gater questions");
        const $t = getRuntimeObject().$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev));
        if (!isRelayNode()) {
            return getRuntimeObject().ret($t.mkVal(Record.mkRecord([
                ['tag', $t.mkVal('Err')],
                ['error', $t.mkVal(Record.mkRecord([['reason', $t.mkVal('NOT_RELAY')]]))],
            ])));
        }
        gaterProcess = arg.val[1];
        gaterPc = $t.pc;
        return getRuntimeObject().ret($t.mkVal(Record.mkRecord([
            ['tag', $t.mkVal('Ok')],
            ['value', $t.mkVal(__unitbase)],
        ])));
    }, "setConnectionGater"),
}
