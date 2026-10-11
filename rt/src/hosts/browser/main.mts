// Browser worker entry point.
//
// Mirrors what rt/src/troupe.mts does for Node, with two differences that follow
// from the platform rather than from choice:
//
//  1. There is no command line, so the options record is seeded directly with
//     setCliArgs() instead of being parsed. As on Node, this must happen before
//     the runtime is imported -- several modules read options at import time --
//     which is why the runtime comes in through a dynamic import.
//
//  2. The scheduler calls setImmediate (Scheduler.mts:178) and is deliberately
//     left unmodified, so the function is installed as a global here. It is
//     backed by MessageChannel: setTimeout(0) is clamped to 4ms, and
//     queueMicrotask would never let the worker drain its message queue.
//
// This entry is incomplete: the runtime still reaches for host facilities that
// do not exist in a browser. Building it is how we measure which ones.

import { setCliArgs, defaultCliArgs, ParsedArgs } from '../../TroupeCliArgs.mjs';

function installSetImmediate(): void {
    const g = globalThis as any;
    if (typeof g.setImmediate === 'function') {
        return;
    }
    const channel = new g.MessageChannel();
    const pending: Array<() => void> = [];
    channel.port1.onmessage = () => {
        const fn = pending.shift();
        if (fn) fn();
    };
    g.setImmediate = (fn: (...args: any[]) => void, ...args: any[]) => {
        pending.push(() => fn(...args));
        channel.port2.postMessage(0);
        return 0;
    };
}

/** Options for a run in the browser. No command line exists to parse. */
export function browserCliArgs(overrides: Partial<ParsedArgs> = {}): ParsedArgs {
    return { ...defaultCliArgs(), ...overrides };
}

export async function boot(args: ParsedArgs) {
    installSetImmediate();
    setCliArgs(args);
    const { start } = await import('../../runtimeMonitored.mjs');
    const { getRuntimeObject } = await import('../../SysState.mjs');
    return { start, getRuntimeObject };
}
