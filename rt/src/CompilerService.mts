// Where the code of an inbound closure comes from.
//
// A received closure carries IR blobs, and the receiving node's own compiler
// turns them into the JavaScript it executes, so the monitor instrumentation
// in that code is the receiver's. Nothing above this interface knows how the
// compiler runs -- a child process on Node, a WebAssembly module elsewhere --
// only that a batch of blobs yields a snippet per blob.

/** One relinked blob: the compiler's `{code, sourceMap?}` for it. */
export interface LinkedSnippet {
    code: string;
    sourceMap?: any;
}

export interface CompilerService {
    /**
     * Relinks one batch of blobs, each the base64 text of a serialized
     * namespace entry, and resolves to one snippet per blob in the same order.
     * Rejects when the compiler fails on the batch. The caller issues one
     * batch at a time.
     */
    linkIR(blobs: string[]): Promise<LinkedSnippet[]>;

    /** Releases the compiler at shutdown. Idempotent. */
    stop(): void;
}

let __compilerService: CompilerService | null = null;

export function registerCompilerService(service: CompilerService): void {
    __compilerService = service;
}

export function getCompilerService(): CompilerService {
    if (__compilerService == null) {
        throw new Error(
            'no compiler service registered: the host must call '
            + 'registerCompilerService() before the runtime deserializes a closure');
    }
    return __compilerService;
}
