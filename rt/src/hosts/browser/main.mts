// The browser worker's bootstrap: what runs before the runtime is imported.
//
// Mirrors rt/src/troupe.mts for Node, with two differences that follow from
// the platform rather than from choice:
//
//  1. There is no command line, so the options record is seeded directly
//     instead of being parsed. As on Node, this must happen before the
//     runtime is imported -- several modules read options at import time --
//     which is why the runtime comes in through a dynamic import.
//
//  2. The scheduler calls setImmediate (Scheduler.mts) and is deliberately
//     left unmodified, so the function is installed as a global here. It is
//     backed by MessageChannel: setTimeout(0) is clamped to 4ms, and
//     queueMicrotask would never let the worker drain its message queue.

import chalk from 'chalk';
import { registerBrowserHost, BrowserHostOptions } from './index.mjs';

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

/**
 * The program troupec emitted, instantiated: the artifact is a CommonJS
 * module whose export is the program's constructor (`module.exports = Top`,
 * Stack2JS.hs), evaluated here as a function body.
 */
export function instantiateProgram(text: string): any {
    const module = { exports: {} as any };
    new Function('module', 'exports', text)(module, module.exports);
    return module.exports;
}

export async function boot(options: BrowserHostOptions) {
    installSetImmediate();
    registerBrowserHost(options);
    // The output sinks are not terminals, so, as on Node writing to a pipe,
    // chalk emits no colour; the logger's own colouring follows --no-color.
    chalk.level = 0;
    const { registerBrowserNatives } = await import('../../ffi/browser/index.mjs');
    registerBrowserNatives();
    const { start } = await import('../../runtimeMonitored.mjs');
    const { getRuntimeObject } = await import('../../SysState.mjs');
    return { start, getRuntimeObject };
}
