// The browser's console for the runtime's reports: log to the stdout sink,
// error to the stderr sink, formatted as Node's console formats a line --
// strings as they are, separated by spaces.
//
// The runtime also reports through the global console in places (the
// scheduler's main-thread line, an error report), which on Node reaches the
// same streams. installConsole() puts this console in the global's place so
// that it does here too; what it does not implement goes to the original.

import { HostConsole } from '../../hostConsole.mjs';
import { Terminal } from '../../Terminal.mjs';

function formatArg(a: any): string {
    if (typeof a === 'string') return a;
    if (a instanceof Error) return a.stack ?? a.message;
    if (typeof a === 'bigint') return `${a}n`;
    if (a === undefined) return 'undefined';
    if (a !== null && typeof a === 'object') {
        try { return JSON.stringify(a); } catch { return String(a); }
    }
    return String(a);
}

export function formatLine(args: any[]): string {
    return args.map(formatArg).join(' ') + '\n';
}

export function browserConsole(terminal: Terminal): HostConsole {
    return {
        log: (...args) => terminal.stdout.write(formatLine(args)),
        error: (...args) => terminal.stderr.write(formatLine(args)),
    };
}

export function installConsole(c: HostConsole): void {
    const original = globalThis.console;
    const replaced = Object.create(original);
    replaced.log = c.log;
    replaced.info = c.log;
    replaced.debug = c.log;
    replaced.warn = c.error;
    replaced.error = c.error;
    (globalThis as any).console = replaced;
}
