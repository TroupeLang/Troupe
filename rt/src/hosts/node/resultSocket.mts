// Node's result channel: newline-delimited JSON over the Unix domain socket
// named by --result-socket. Without the option the channel is disabled.

import * as net from 'node:net';
import { getCliArgs, TroupeCliArg } from '../../TroupeCliArgs.mjs';
import { ResultChannel, ResultMessage } from '../../ResultChannel.mjs';

let client: net.Socket | null = null;

function socketPath(): string | undefined {
    return getCliArgs()[TroupeCliArg.ResultSocket] as string | undefined;
}

/**
 * Connects to the socket. On connection error: warns to stderr and resolves,
 * never blocking startup. The socket is unref()'d so it does not keep the
 * process alive.
 */
function connect(): Promise<void> {
    const path = socketPath();
    if (!path) return Promise.resolve();

    return new Promise<void>((resolve) => {
        client = net.createConnection({ path }, () => {
            client!.unref();
            resolve();
        });
        client.on('error', (err) => {
            console.error(`Warning: could not connect to result socket ${path}: ${err.message}`);
            client = null;
            resolve();
        });
    });
}

/**
 * Writes a JSON message to the socket, handling backpressure.
 * If write() returns false (kernel buffer full), waits for the 'drain' event.
 */
async function writeMessage(msg: ResultMessage): Promise<void> {
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

async function send(msg: ResultMessage): Promise<void> {
    if (!client || client.destroyed || client.writableEnded) return;
    try {
        await writeMessage(msg);
    } catch (err) {
        const m = err instanceof Error ? err.message : String(err);
        console.error(`Warning: failed to write to result socket: ${m}`);
    }
}

async function sendAndClose(msg: ResultMessage): Promise<void> {
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

export const nodeResultSocket: ResultChannel = {
    enabled: () => !!socketPath(),
    connect,
    send,
    sendAndClose,
};
