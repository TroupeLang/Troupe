// The page's side of a browser Troupe node: starting the compiler worker and
// a runtime worker, and the wiring between them. Bundled as
// build/browser/troupe-page.mjs (make page-client) for a page to import.

import { browserCompilerService, BrowserCompilerService } from './compilerService.mjs';
import { RunRequest, WorkerMessage } from './protocol.mjs';
import { ResultMessage } from '../../ResultChannel.mjs';

export { browserCompilerService };
export { STAGES } from './stages.mjs';
export type { RunRequest, WorkerMessage, BrowserCompilerService };

/** The library interfaces the compiler reads for `import`, fetched from a
 *  manifest (a JSON list of file names) and a base URL. */
export async function fetchInterfaces(manifestUrl: string, baseUrl: string): Promise<Array<[string, Uint8Array]>> {
    const names: string[] = await (await fetch(manifestUrl)).json();
    return Promise.all(names.map(async (n) => {
        const r = await fetch(baseUrl + n);
        return [n, new Uint8Array(await r.arrayBuffer())] as [string, Uint8Array];
    }));
}

/** Starts the compiler worker and hands it the compiler and the interfaces. */
export async function startCompiler(
    workerUrl: string, wasmUrl: string, interfaces: Array<[string, Uint8Array]>,
): Promise<BrowserCompilerService> {
    const worker = new Worker(workerUrl, { type: 'module' });
    const service = browserCompilerService(worker);
    const wasm = await (await fetch(wasmUrl)).arrayBuffer();
    await service.init(wasm, interfaces);
    return service;
}

/** A port to the compiler for a runtime worker's run request. */
export async function compilerPort(service: BrowserCompilerService): Promise<MessagePort> {
    const { port1, port2 } = new MessageChannel();
    await service.serve(port2);
    return port1;
}

export interface NodeHandlers {
    onStdout?(text: string): void;
    onStderr?(text: string): void;
    onResult?(value: string): void;
    onError?(message: string): void;
}

export interface RunningNode {
    /** Delivers one input line to the program. */
    feedLine(line: string): void;
    /** Stops the node unconditionally. */
    terminate(): void;
    /** The node's exit message, or a rejection when the worker fails to load. */
    exit: Promise<ResultMessage>;
}

/** Starts a runtime worker on the request, with the program's output and
 *  lifecycle delivered to the handlers. */
export function startNode(workerUrl: string, request: RunRequest, handlers: NodeHandlers): RunningNode {
    const worker = new Worker(workerUrl, { type: 'module' });
    const exit = new Promise<ResultMessage>((resolve, reject) => {
        worker.addEventListener('message', (ev) => {
            const m = ev.data as WorkerMessage;
            switch (m.type) {
                case 'stdout': handlers.onStdout?.(m.text); break;
                case 'stderr': handlers.onStderr?.(m.text); break;
                case 'main-thread-result': handlers.onResult?.(m.value ?? ''); break;
                case 'error': handlers.onError?.(m.message ?? ''); break;
                case 'process-exit': resolve(m); break;
            }
        });
        worker.addEventListener('error', (ev) => reject(new Error(ev.message)));
    });
    worker.postMessage(request, request.compiler ? [request.compiler] : []);
    return {
        feedLine: (line) => worker.postMessage({ op: 'stdin', line }),
        terminate: () => worker.terminate(),
        exit,
    };
}
