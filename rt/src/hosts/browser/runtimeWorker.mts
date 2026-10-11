// The runtime worker: one Troupe node, served over a message port.
//
// The page sends a run request and, for a program that reads input, lines;
// the worker posts program output and lifecycle messages (protocol.mts).
// The worker runs one program: the runtime is a process-level singleton, and
// a page that wants another run starts another worker.

import { MessagePortLike, PageMessage, RunRequest, WorkerMessage } from './protocol.mjs';
import { browserTerminal } from './terminal.mjs';

export function serveRuntime(port: MessagePortLike, close: (code: number) => void): void {
    const post = (msg: WorkerMessage) => port.postMessage(msg);
    const { terminal, feedLine } = browserTerminal((kind, text) => post({ type: kind, text }));
    let started = false;

    async function run(req: RunRequest): Promise<void> {
        const { boot, instantiateProgram } = await import('./main.mjs');
        const { start, getRuntimeObject } = await boot({
            args: req.args ?? {},
            terminal,
            code: { libraries: req.code.libraries, service: req.code.service, sources: req.sources ?? {} },
            config: req.config ?? {},
            post,
            close,
            supervised: req.supervised ?? true,
            storage: req.storage ?? 'troupe',
            compiler: req.compiler ?? null,
        });
        const Top = instantiateProgram(req.program);
        const top = new Top((getRuntimeObject() as any).__userRuntime);
        start(top);
    }

    port.addEventListener('message', (ev) => {
        const msg = ev.data as PageMessage;
        switch (msg.op) {
            case 'run':
                if (started) {
                    post({ type: 'error', message: 'this worker is already running a program' });
                    return;
                }
                started = true;
                run(msg).catch((e) => {
                    post({ type: 'error', message: e instanceof Error ? (e.stack ?? e.message) : String(e) });
                    post({ type: 'process-exit', exitCode: 1 });
                    close(1);
                });
                return;
            case 'stdin':
                feedLine(msg.line);
                return;
        }
    });
    port.start?.();
}
