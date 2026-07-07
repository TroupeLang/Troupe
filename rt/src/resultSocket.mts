'use strict'
import * as net from 'node:net';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';

export interface SocketMessage {
    type: 'main-thread-result' | 'process-exit' | 'error';
    value?: string;
    exitCode?: number;
    reason?: string;
    message?: string;
}

let client: net.Socket | null = null;

/**
 * Returns true if --result-socket was provided on the command line.
 */
export function isResultSocketEnabled(): boolean {
    return !!getCliArgs()[TroupeCliArg.ResultSocket];
}

/**
 * Connects to the Unix socket specified by --result-socket.
 * No-op if --result-socket was not provided.
 * On connection error: warns to stderr and resolves (never blocks startup).
 * The socket is unref()'d so it doesn't prevent natural process exit.
 */
export function connectResultSocket(): Promise<void> {
    const socketPath = getCliArgs()[TroupeCliArg.ResultSocket];
    if (!socketPath) return Promise.resolve();

    return new Promise<void>((resolve) => {
        client = net.createConnection({ path: socketPath }, () => {
            client!.unref();
            resolve();
        });
        client.on('error', (err) => {
            console.error(`Warning: could not connect to result socket ${socketPath}: ${err.message}`);
            client = null;
            resolve();
        });
    });
}

/**
 * Writes a JSON message to the socket, handling backpressure.
 * If write() returns false (kernel buffer full), waits for the 'drain' event.
 */
async function writeMessage(msg: SocketMessage): Promise<void> {
    if (!client || client.destroyed || client.writableEnded) return;
    client.ref();  // Ensure the event loop stays alive for the write
    const data = JSON.stringify(msg) + '\n';
    return new Promise<void>((resolve, reject) => {
        const ok = client!.write(data);
        if (ok) {
            resolve();
        } else {
            client!.once('drain', resolve);
            client!.once('error', reject);
        }
    });
}

/**
 * Sends a message over the socket. No-op if not connected.
 * Catches and logs errors without throwing.
 */
export async function sendSocketMessage(msg: SocketMessage): Promise<void> {
    if (!client || client.destroyed || client.writableEnded) return;
    try {
        await writeMessage(msg);
    } catch (err) {
        const m = err instanceof Error ? err.message : String(err);
        console.error(`Warning: failed to write to result socket: ${m}`);
    }
}

/**
 * Sends a final message, flushes, and closes the connection.
 * Used for the process-exit message before process.exit().
 */
export async function sendSocketMessageAndClose(msg: SocketMessage): Promise<void> {
    if (!client || client.destroyed || client.writableEnded) return;
    try {
        await writeMessage(msg);
    } catch (err) {
        const m = err instanceof Error ? err.message : String(err);
        console.error(`Warning: failed to write to result socket: ${m}`);
    }
    return new Promise<void>((resolve) => {
        if (!client || client.destroyed || client.writableEnded) {
            resolve();
            return;
        }
        client.end(() => {
            resolve();
        });
    });
}
