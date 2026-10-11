// The console the runtime reports on: thread errors, handler warnings and
// the debugging dump of the scheduler's state. A host supplies one with the
// output streams and colour mode it wants; the global console is the default.

export interface HostConsole {
    log(...args: any[]): void;
    error(...args: any[]): void;
}

let __hostConsole: HostConsole = globalThis.console;

export function registerHostConsole(c: HostConsole): void {
    __hostConsole = c;
}

export function getHostConsole(): HostConsole {
    return __hostConsole;
}
