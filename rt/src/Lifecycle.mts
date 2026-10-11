// How a node ends. The runtime asks the host to end the node with an exit
// code once it has sent its final message and run its cleanup; what ending
// means -- a process exit, a worker closing -- is the host's.

export interface Lifecycle {
    /** Ends the node with the code. Does not return on Node; on a host
     *  where ending is asynchronous, nothing of the runtime runs after it. */
    exit(code: number): void;
}

let __lifecycle: Lifecycle | null = null;

export function registerLifecycle(l: Lifecycle): void {
    __lifecycle = l;
}

export function getLifecycle(): Lifecycle {
    if (__lifecycle == null) {
        throw new Error(
            'no lifecycle registered: the host must call registerLifecycle() '
            + 'before the runtime ends the node');
    }
    return __lifecycle;
}
