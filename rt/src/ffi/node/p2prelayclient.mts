'use strict'
import type { PeerId } from '@libp2p/interface';
import { peerIdFromString } from '@libp2p/peer-id';
import { multiaddr } from '@multiformats/multiaddr';
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { lub, BOT, Level } from '../../Level.mjs';
import { assertIsLocalProcessId, assertIsNTuple, assertIsRootAuthority, assertIsString, assertNormalState } from '../../Asserts.mjs'
import { __unitbase } from '../../UnitBase.mjs';
import { mkTuple } from '../../ValuesUtil.mjs';
import { getRuntimeObject } from '../../SysState.mjs';
import { checkEffectAtBot } from './p2prelaycontrol.mjs';
import { libp2pNode, addRelayToPeerStore, useRelayIfUnset } from '../../p2p/p2p.mjs';

/**
 * P2PRelayClient — a node's side of relay reservations (manifest
 * ffi/P2PRelayClient.exports, library lib/P2PRelayClient.trp).
 *
 * `reserve` asks a relay for a reservation and suspends the calling thread
 * until the relay answers. `setRelayEvents` names the local process that
 * receives `("relayReservation", relay, expiresAt)` and
 * `("relayReservationLost", relay)` from the circuit relay transport's
 * reservation store, and `("relayDialFailed", peer, relay, status)` from the
 * runtime's dial loop (p2p.mts, `dial`), one message per failed attempt.
 */

// The time the circuit relay transport gives a reservation
// (DEFAULT_RESERVATION_COMPLETION_TIMEOUT, circuit-relay-v2 4.2.14 constants.js:19).
const RESERVATION_TIMEOUT = 5_000;

const CIRCUIT_RELAY_TRANSPORT = '@libp2p/circuit-relay-v2-transport';

/**
 * The parts of @libp2p/circuit-relay-v2 4.2.14 that its package does not
 * export: the circuit relay transport's field `reservationStore`
 * (transport/index.js:37,60), tagged as CIRCUIT_RELAY_TRANSPORT (:70), and the
 * store's `addRelay` (transport/reservation-store.js:117), which resolves with
 * the reservation (:245-252) and rejects with "reservation failed with status
 * <status>" on a refusal (:338-340), and its events `relay:created-reservation`
 * (:249) and `relay:removed` (:360). libp2p 3.3.12 reaches the transport
 * through the transport manager's `getTransports` (transport-manager.js:110)
 * on the node's field `components` (libp2p.js:34,59), which the node's declared
 * type lacks. tests/libp2p-contract checks them against the installed version.
 */
interface TransportReservation {
    relay: PeerId;
    details: { reservation: { expire: bigint } };
}

interface TransportReservationStore {
    addRelay(peer: PeerId, type: 'configured'): Promise<TransportReservation>;
    addEventListener(type: 'relay:created-reservation' | 'relay:removed',
                     listener: (evt: CustomEvent<TransportReservation>) => void): void;
}

interface NodeInternals {
    components: { transportManager: { getTransports(): any[] } };
}

function transportReservationStore(): TransportReservationStore | null {
    const node = libp2pNode() as unknown as NodeInternals | null;
    if (node === null) {
        return null;
    }
    const transport = node.components.transportManager.getTransports()
        .find((t) => t[Symbol.toStringTag] === CIRCUIT_RELAY_TRANSPORT);
    return transport?.reservationStore ?? null;
}

let eventsProcess: LVal | null = null;
let eventsPc: Level = BOT;
let forwarding = false;

function deliver(...fields: (string | number)[]): void {
    if (eventsProcess === null) {
        return;
    }
    const pc = eventsPc;
    const msg = new LVal(mkTuple(fields.map((f) => new LVal(f, pc))), pc);
    getRuntimeObject().__mbox.enqueueFromHost(eventsProcess, msg, pc);
}

function expiresAt(r: TransportReservation): number {
    return Number(r.details.reservation.expire) * 1000;
}

function forwardReservationEvents(store: TransportReservationStore): void {
    if (forwarding) {
        return;
    }
    forwarding = true;
    store.addEventListener('relay:created-reservation', (evt) =>
        deliver("relayReservation", evt.detail.relay.toString(), expiresAt(evt.detail)));
    store.addEventListener('relay:removed', (evt) =>
        deliver("relayReservationLost", evt.detail.relay.toString()));
}

// The status a relay returned to a dial through it; the circuit relay
// transport puts it in the message "failed to connect via relay with status
// <status>" (transport/index.js:150). A dial over several addresses fails with
// an AggregateError of one error per address (libp2p dial-queue.js:261-263).
function relayStatus(err: any): string | null {
    const errors = err instanceof AggregateError ? err.errors : [err];
    for (const e of errors) {
        const m = /via relay with status ([A-Z_]+)/.exec(e?.message ?? '');
        if (m) {
            return m[1];
        }
    }
    return null;
}

/**
 * Called by the dial loop (p2p.mts, `dial`) on each failed attempt to dial
 * `peer` while `relayId` is the relay its circuit address goes through.
 */
export function relayDialFailed(peer: PeerId, relayId: string | null, err: any): void {
    if (relayId === null) {
        return;
    }
    deliver("relayDialFailed", peer.toString(), relayId,
        relayStatus(err) ?? err?.name ?? err?.code ?? String(err));
}

function isTimeout(err: any): boolean {
    return err?.name === 'TimeoutError' || err?.name === 'AbortError';
}

function okRecord(value: LVal, pc: Level): LVal {
    return new LVal(Record.mkRecord([
        ['tag', new LVal('Ok', pc)],
        ['value', value],
    ]), pc);
}

function errRecord(pc: Level, reason: string, message: string | null = null): LVal {
    const fields: [string, LVal][] = [['reason', new LVal(reason, pc)]];
    if (message !== null) {
        fields.push(['message', new LVal(message, pc)]);
    }
    return new LVal(Record.mkRecord([
        ['tag', new LVal('Err', pc)],
        ['error', new LVal(Record.mkRecord(fields), pc)],
    ]), pc);
}

function relayPeerOf(addr: LVal): PeerId {
    try {
        const p2p = multiaddr(addr.val).getComponents().filter((c) => c.name === 'p2p').pop();
        if (p2p?.value === undefined) {
            throw new Error('no /p2p/<id> component');
        }
        return peerIdFromString(p2p.value);
    } catch (e) {
        getRuntimeObject().$t.threadError(`${addr.stringRep()} is not a relay address: ${e.message}`);
    }
}

// The reservation goes to the store's `addRelay`, not to the transport
// manager's `listen` on `<relay>/p2p-circuit`: under the transport manager's
// fault tolerance NO_FATAL, Troupe's default, `listen` resolves when the relay
// refuses (libp2p transport-manager.js:233-236; tests/libp2p-contract). The
// relay is dialled first so that a dial failure never reaches `addRelay`,
// which would bar the relay from every later `addRelay`
// (transport/reservation-store.js:130-132, :260-261).
async function reserveOn(store: TransportReservationStore, addr: string, relay: PeerId, pc: Level): Promise<LVal> {
    await addRelayToPeerStore(addr, relay);
    try {
        await libp2pNode().dial(relay, { signal: AbortSignal.timeout(RESERVATION_TIMEOUT) });
    } catch (e) {
        return errRecord(pc, isTimeout(e) ? 'TIMEOUT' : 'DIAL');
    }
    useRelayIfUnset(relay);
    try {
        const r = await store.addRelay(relay, 'configured');
        return okRecord(new LVal(Record.mkRecord([
            ['relay', new LVal(r.relay.toString(), pc)],
            ['expiresAt', new LVal(expiresAt(r), pc)],
        ]), pc), pc);
    } catch (e) {
        if (/status PERMISSION_DENIED/.test(e?.message ?? '')) {
            return errRecord(pc, 'PERMISSION_DENIED');
        }
        return isTimeout(e) ? errRecord(pc, 'TIMEOUT') : errRecord(pc, 'FAILED', `${e?.name}: ${e?.message}`);
    }
}

export const p2pRelayClientExports = {
    reserve: mkBase((arg) => {
        assertNormalState("reserve");
        assertIsNTuple(arg, 2);
        assertIsRootAuthority(arg.val[0]);
        assertIsString(arg.val[1]);
        const rt = getRuntimeObject();
        const $t = rt.$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev));
        const relay = relayPeerOf(arg.val[1]);
        const store = transportReservationStore();
        if (store === null) {
            return rt.ret(errRecord($t.pc, 'NO_NETWORK'));
        }
        checkEffectAtBot("reserve above the relay's channel level", BOT);
        const theThread = $t;
        const pc = $t.pc;
        (async () => {
            let result: LVal;
            try {
                result = await reserveOn(store, arg.val[1].val, relay, pc);
            } catch (e) {
                result = errRecord(pc, 'FAILED', `${e?.name}: ${e?.message}`);
            }
            theThread.returnSuspended(result);
            rt.__sched.scheduleThread(theThread);
            rt.__sched.resumeLoopAsync();
        })();
    }, "reserve"),

    setRelayEvents: mkBase((arg) => {
        assertNormalState("setRelayEvents");
        assertIsNTuple(arg, 2);
        assertIsRootAuthority(arg.val[0]);
        assertIsLocalProcessId(arg.val[1], "relay events");
        const rt = getRuntimeObject();
        const $t = rt.$t;
        $t.raiseCurrentThreadPC(lub(arg.lev, arg.val[0].lev, arg.val[1].lev));
        const store = transportReservationStore();
        if (store === null) {
            return rt.ret(errRecord($t.pc, 'NO_NETWORK'));
        }
        eventsProcess = arg.val[1];
        eventsPc = $t.pc;
        forwardReservationEvents(store);
        return rt.ret(okRecord(new LVal(__unitbase, $t.pc), $t.pc));
    }, "setRelayEvents"),
}
