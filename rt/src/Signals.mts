// The operating system's termination signal, as the signal builtins reach
// it. A host that has no signals registers an implementation whose trap is
// never called: nothing is ever delivered, and the builtins' bookkeeping is
// unchanged.

export interface Signals {
    /** Attaches the handler to the termination signal. */
    trapTerminate(handler: () => void): void;

    /** Detaches a handler attached by trapTerminate. */
    untrapTerminate(handler: () => void): void;

    /** Ends the node the way an untrapped termination signal does. Called
     *  with no handler attached. */
    terminateByDefault(): void;
}

let __signals: Signals | null = null;

export function registerSignals(s: Signals): void {
    __signals = s;
}

export function getSignals(): Signals {
    if (__signals == null) {
        throw new Error('no signals registered on this host');
    }
    return __signals;
}
