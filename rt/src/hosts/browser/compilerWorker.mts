// The compiler worker: one TroupecWasm, served over a message port.
//
// The Haskell runtime is single-threaded, so a worker compiles one request
// at a time; requests queue in arrival order. Relinked blobs are memoized by
// blob: closure blobs repeat across a node's traffic, and a blob's snippet
// depends on nothing but the blob.

import { TroupecWasm, CompileResult, LinkedBlob } from './troupecWasm.mjs';

/** What both sides need of a port: a browser Worker's global scope, a
 *  MessagePort, or Node's. */
export interface MessagePortLike {
    postMessage(message: any, transfer?: any[]): void;
    addEventListener(type: 'message', listener: (ev: { data: any }) => void): void;
    start?(): void;
}

export type CompilerRequest =
    | { id: number; op: 'init'; wasm: ArrayBuffer; interfaces: Array<[string, Uint8Array]> }
    | { id: number; op: 'compileSource'; source: string }
    | { id: number; op: 'linkIR'; blobs: string[] }
    /** Serve the same compiler on another port as well: the one a runtime
     *  worker holds, so a node links through the compiler its page loaded. */
    | { id: number; op: 'serve'; port: MessagePortLike };

export type CompilerReply =
    | { id: number; ok: true; result: any }
    | { id: number; ok: false; error: string };

export interface CompilerServer {
    /** Answers requests arriving on the port. */
    serve(port: MessagePortLike): void;
}

/** One compiler, served on as many ports as are attached. */
export function createCompilerServer(): CompilerServer {
    let compiler: TroupecWasm | null = null;
    const linked = new Map<string, LinkedBlob>();
    let queue: Promise<void> = Promise.resolve();

    async function handle(req: CompilerRequest): Promise<any> {
        switch (req.op) {
            case 'init':
                compiler = await TroupecWasm.fromBytes(req.wasm, req.interfaces);
                linked.clear();
                return null;
            case 'serve':
                serve(req.port);
                return null;
            case 'compileSource':
                return await requireCompiler().compileSource(req.source);
            case 'linkIR': {
                const missing = req.blobs.filter((b) => !linked.has(b));
                if (missing.length > 0) {
                    const r = await requireCompiler().linkBlobs(missing);
                    if (!r.ok) {
                        throw new Error(r.crash ?? `troupec --json-ir exited ${r.code}: ${r.errors}`);
                    }
                    if (r.snippets.length !== missing.length || r.snippets.some((s) => s.malformed)) {
                        throw new Error('troupec --json-ir returned a malformed batch');
                    }
                    missing.forEach((b, i) => linked.set(b, r.snippets[i]));
                }
                return req.blobs.map((b) => linked.get(b));
            }
        }
    }

    function requireCompiler(): TroupecWasm {
        if (compiler == null) throw new Error('compiler worker: not initialized');
        return compiler;
    }

    function serve(port: MessagePortLike): void {
        port.addEventListener('message', (ev) => {
            const req = ev.data as CompilerRequest;
            queue = queue.then(async () => {
                let reply: CompilerReply;
                try {
                    reply = { id: req.id, ok: true, result: await handle(req) };
                } catch (e) {
                    reply = { id: req.id, ok: false, error: e instanceof Error ? e.message : String(e) };
                }
                port.postMessage(reply);
            });
        });
        port.start?.();
    }

    return { serve };
}

export function serveCompiler(port: MessagePortLike): void {
    createCompilerServer().serve(port);
}

export type { CompileResult };
