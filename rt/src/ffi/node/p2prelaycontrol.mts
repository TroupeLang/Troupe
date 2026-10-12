'use strict'
import type { PeerId } from '@libp2p/interface';
import type { CircuitRelayService, RelayReservation } from '@libp2p/circuit-relay-v2';
import type { PeerMap } from '@libp2p/peer-collections';
import { peerIdFromString } from '@libp2p/peer-id';
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { lub, flowsTo, BOT, Level } from '../../Level.mjs';
import { assertIsBoolean, assertIsLocalProcessId, assertIsNTuple, assertIsNumber, assertIsRecord, assertIsRootAuthority, assertIsString, assertNormalState, rawAssertRecordHasField } from '../../Asserts.mjs'
import { __unitbase } from '../../UnitBase.mjs';
import { mkList, mkTuple } from '../../ValuesUtil.mjs';
import { getRuntimeObject } from '../../SysState.mjs';
import { ErrorKind } from '../../TroupeError.mjs';
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
 *
 * `setLimits`, `removeReservation` and `reservations` act on the libp2p relay
 * service's reservation store, which p2p.mts hands over with `setRelayService`.
 */

const DENY = true;

let gaterProcess: LVal | null = null;
let gaterPc: Level = BOT;

/**
 * Fields of @libp2p/circuit-relay-v2 4.2.14 that its declarations mark private:
 * the service's `reservationStore` (server/index.js:19,39) and the store's four
 * limits (server/reservation-store.js:7-11), which `reserve` reads at each
 * reservation (:27-36, :40, :48). tests/libp2p-contract checks them against the
 * installed version.
 */
interface ReservationStoreInternals {
    reservations: PeerMap<RelayReservation>;
    maxReservations: number;
    reservationTtl: number;
    defaultDurationLimit: number;
    defaultDataLimit: bigint;
    get(peer: PeerId): RelayReservation | undefined;
    removeReservation(peer: PeerId): void;
}

let reservationStore: ReservationStoreInternals | null = null;

export function setRelayService(service: CircuitRelayService): void {
    reservationStore = (service as unknown as { reservationStore: ReservationStoreInternals }).reservationStore;
}

function isRelayNode(): boolean {
    return getCliArgs()[TroupeCliArg.StartRelay] !== undefined;
}

// The check of checkChannelEffect (builtins/stdio.mts) against BOT: the effect
// of the call is seen by the relay's peers. `beforeError` runs before the
// thread error.
function checkEffectAtBot(operation: string, operandLevel: Level, beforeError: () => void = () => {}) {
    const $t = getRuntimeObject().$t;
    $t.raiseCurrentThreadPCToBlockingLev();
    const effectLevel = lub($t.pc, operandLevel);
    if (!flowsTo(effectLevel, BOT)) {
        beforeError();
        $t.threadError(
            `${operation}\n` +
            ` | effect level  ${effectLevel.stringRep()}\n` +
            ` | pc level      ${$t.pc.stringRep()}\n` +
            ` | channel level ${BOT.stringRep()}`,
            false, null, ErrorKind.IFCCheck);
    }
}

function ok(value: LVal): any {
    const $t = getRuntimeObject().$t;
    return getRuntimeObject().ret($t.mkVal(Record.mkRecord([
        ['tag', $t.mkVal('Ok')],
        ['value', value],
    ])));
}

function notRelay(): any {
    const $t = getRuntimeObject().$t;
    return getRuntimeObject().ret($t.mkVal(Record.mkRecord([
        ['tag', $t.mkVal('Err')],
        ['error', $t.mkVal(Record.mkRecord([['reason', $t.mkVal('NOT_RELAY')]]))],
    ])));
}

function assertIsPeerId(x: LVal): PeerId {
    assertIsString(x);
    try {
        return peerIdFromString(x.val);
    } catch (e) {
        getRuntimeObject().$t.threadError(`${x.stringRep()} is not a peer id: ${e.message}`);
    }
}

// A limit is a whole number of milliseconds or bytes; `least` is its smallest value.
function limitField(record: Record, name: string, least: number): number {
    rawAssertRecordHasField(record, name);
    const field = record.getField(name);
    assertIsNumber(field);
    if (!Number.isSafeInteger(field.val) || field.val < least) {
        getRuntimeObject().$t.threadError(
            `setLimits: ${name} is ${field.val}; it must be an integer of at least ${least}`);
    }
    return field.val;
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
        // A failure resolves the question with a deny before the thread error,
        // so libp2p never waits on a caller that has died.
        checkEffectAtBot("reply above the answer's channel level", arg.lev, () => resolve(DENY));
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
    return new LVal(peer.toString(), pc);
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
            return notRelay();
        }
        gaterProcess = arg.val[1];
        gaterPc = $t.pc;
        return ok($t.mkVal(__unitbase));
    }, "setConnectionGater"),

    setLimits: mkBase((arg) => {
        assertNormalState("setLimits");
        assertIsNTuple(arg, 2);
        assertIsRootAuthority(arg.val[0]);
        assertIsRecord(arg.val[1]);
        const record: Record = arg.val[1].val;
        const limits = {
            reservationTtl: limitField(record, 'reservationTtl', 1),
            maxReservations: limitField(record, 'maxReservations', 0),
            defaultDurationLimit: limitField(record, 'defaultDurationLimit', 1),
            defaultDataLimit: limitField(record, 'defaultDataLimit', 1),
        };
        const $t = getRuntimeObject().$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev,
            ...Object.keys(limits).map((name) => record.getField(name).lev)));
        if (reservationStore === null) {
            return notRelay();
        }
        checkEffectAtBot("setLimits above the relay's channel level", BOT);
        reservationStore.reservationTtl = limits.reservationTtl;
        reservationStore.maxReservations = limits.maxReservations;
        reservationStore.defaultDurationLimit = limits.defaultDurationLimit;
        reservationStore.defaultDataLimit = BigInt(limits.defaultDataLimit);
        return ok($t.mkVal(__unitbase));
    }, "setLimits"),

    removeReservation: mkBase((arg) => {
        assertNormalState("removeReservation");
        assertIsNTuple(arg, 3);
        assertIsRootAuthority(arg.val[0]);
        const peer = assertIsPeerId(arg.val[1]);
        assertIsBoolean(arg.val[2]);
        const $t = getRuntimeObject().$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev, arg.val[2].lev));
        if (reservationStore === null) {
            return notRelay();
        }
        checkEffectAtBot("removeReservation above the relay's channel level", BOT);
        const reservation = reservationStore.get(peer);
        reservationStore.removeReservation(peer);
        // Removal alone leaves the peer's relayed connections open; aborting the
        // reservation's signal, which createLimitedRelay (utils.js:27-34) joins to
        // each of them, ends them. Its abort listener deletes the entry again
        // (server/reservation-store.js:53-55).
        if (arg.val[2].val && reservation !== undefined) {
            reservation.signal.reset(0);
        }
        return ok($t.mkVal(__unitbase));
    }, "removeReservation"),

    reservations: mkBase((arg) => {
        assertNormalState("reservations");
        assertIsRootAuthority(arg);
        const $t = getRuntimeObject().$t;
        $t.raiseCurrentThreadPC(arg.lev);
        if (reservationStore === null) {
            return notRelay();
        }
        const entries = [...reservationStore.reservations.entries()].map(([peer, r]) =>
            $t.mkVal(Record.mkRecord([
                ['peer', $t.mkVal(peer.toString())],
                ['expiresAt', $t.mkVal(r.expiry.getTime())],
                ['durationLimit', $t.mkVal(r.limit?.duration ?? 0)],
                ['dataLimit', $t.mkVal(Number(r.limit?.data ?? 0n))],
            ])));
        return ok($t.mkVal(mkList(entries)));
    }, "reservations"),
}
