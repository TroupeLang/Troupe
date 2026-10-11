// The browser's compiler service: the runtime's side of the compiler worker.
//
// linkIR is what deserialize.mts calls to relink an inbound closure; the
// worker answers with one snippet per blob. compileSource is the same
// compiler on Troupe source, for a page that compiles what it runs.

import { CompilerService, LinkedSnippet } from '../../CompilerService.mjs';
import { CompilerReply, CompilerRequest, MessagePortLike } from './compilerWorker.mjs';
import { CompileResult, CompileOptions, ProjectResult } from './troupecWasm.mjs';

export interface BrowserCompilerService extends CompilerService {
    /** Sends the compiler and the library interfaces to the worker. */
    init(wasm: ArrayBuffer, interfaces: Array<[string, Uint8Array]>): Promise<void>;
    compileSource(source: string, options?: CompileOptions): Promise<CompileResult>;
    /** Compiles a project of several files, `main` among them (troupecWasm.mts). */
    compileProject(files: Array<[string, string]>, main: string, options?: CompileOptions): Promise<ProjectResult>;
    /** Has the worker serve the same compiler on the port as well, which is
     *  transferred: a runtime worker given its other end links through it. */
    serve(port: MessagePort): Promise<void>;
}

export function browserCompilerService(port: MessagePortLike): BrowserCompilerService {
    let nextId = 1;
    const pending = new Map<number, { resolve: (r: any) => void; reject: (e: Error) => void }>();

    port.addEventListener('message', (ev) => {
        const reply = ev.data as CompilerReply;
        const p = pending.get(reply.id);
        if (!p) return;
        pending.delete(reply.id);
        if (reply.ok === true) {
            p.resolve(reply.result);
        } else {
            p.reject(new Error(reply.error));
        }
    });
    port.start?.();

    type RequestBody = {
        [K in CompilerRequest['op']]: Omit<Extract<CompilerRequest, { op: K }>, 'id'>
    }[CompilerRequest['op']];

    function request(req: RequestBody, transfer: any[] = []): Promise<any> {
        const id = nextId++;
        return new Promise((resolve, reject) => {
            pending.set(id, { resolve, reject });
            port.postMessage({ id, ...req }, transfer);
        });
    }

    return {
        init: (wasm, interfaces) => request({ op: 'init', wasm, interfaces }),
        compileSource: (source, options = {}) => request({ op: 'compileSource', source, options }),
        compileProject: (files, main, options = {}) => request({ op: 'compileProject', files, main, options }),
        linkIR: (blobs: string[]): Promise<LinkedSnippet[]> => request({ op: 'linkIR', blobs }),
        serve: (p: MessagePort) => request({ op: 'serve', port: p }, [p]),
        stop: () => {},
    };
}
