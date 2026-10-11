// Where the runtime's lifecycle messages go.
//
// A supervisor of a node -- a test harness, a page -- learns the main
// thread's result and the node's exit through structured messages rather
// than by parsing output. Nothing above this interface knows the transport:
// a Unix socket on Node, a message port elsewhere.

export interface ResultMessage {
    type: 'main-thread-result' | 'process-exit' | 'error';
    value?: string;
    exitCode?: number;
    reason?: string;
    message?: string;
}

export interface ResultChannel {
    /** Whether a supervisor is listening. When one is, the console copy of
     *  the main thread's result is not printed. */
    enabled(): boolean;

    /** Opens the channel before the program starts. Never rejects: a channel
     *  that cannot open reports it and stays disabled. */
    connect(): Promise<void>;

    /** Sends one message; a no-op when the channel is not open. */
    send(msg: ResultMessage): Promise<void>;

    /** Sends a final message and closes the channel. */
    sendAndClose(msg: ResultMessage): Promise<void>;
}

/** A node nobody supervises: every message is dropped. */
export const noResultChannel: ResultChannel = {
    enabled: () => false,
    connect: async () => {},
    send: async () => {},
    sendAndClose: async () => {},
};

let __resultChannel: ResultChannel = noResultChannel;

export function registerResultChannel(channel: ResultChannel): void {
    __resultChannel = channel;
}

export function getResultChannel(): ResultChannel {
    return __resultChannel;
}
