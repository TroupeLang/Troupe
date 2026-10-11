// Node's compiler service: `bin/troupec --json-ir` as a child process that
// lives as long as the node.
//
// The child is started when the service is constructed, before the first
// program runs, and its presence keeps the event loop alive: a program that
// deadlocks hangs instead of exiting 0, which the golden harness relies on.
// If the child dies before the runtime initiated its own exit, the node exits
// with the child's code.

import { spawn } from 'node:child_process';
import { getTroupeRoot } from './troupeRoot.mjs';
import { CompilerService, LinkedSnippet } from '../../CompilerService.mjs';

// The runtime brackets a batch with an !ECHO line that the compiler echoes
// back after the batch's snippets; everything before it is the batch's
// output. The compiler's output arrives in whatever chunks the pipe delivers,
// so it is accumulated until the marker shows.
const MARKER = "/*-----*/\n\n";

type Pending = {
    count: number;
    resolve: (snippets: LinkedSnippet[]) => void;
    reject: (e: unknown) => void;
};

export function startNodeCompiler(exitInitiated: () => boolean): CompilerService {
    const child = spawn(getTroupeRoot() + '/bin/troupec', ['--json-ir']);
    child.on('exit', (code: number) => {
        if (!exitInitiated()) {
            process.exit(code);
        }
    });

    let accum = "";
    let pending: Pending | null = null;

    child.stdout.on('data', (data: string) => {
        accum += data;
        const j = accum.indexOf(MARKER);
        if (j >= 0) {
            const output = accum.slice(0, j);
            accum = accum.slice(j + MARKER.length);
            const p = pending;
            pending = null;
            if (p) {
                try {
                    // Snippets are separated by blank lines; one per blob, in
                    // order. A snippet that does not parse rejects the batch.
                    const parts = output.split("\n\n");
                    const snippets: LinkedSnippet[] = [];
                    for (let k = 0; k < p.count; k++) {
                        snippets.push(JSON.parse(parts[k]));
                    }
                    p.resolve(snippets);
                } catch (e) {
                    p.reject(e);
                }
            }
        }
    });

    return {
        linkIR(blobs: string[]): Promise<LinkedSnippet[]> {
            return new Promise((resolve, reject) => {
                if (pending) {
                    reject(new Error('compiler service: a batch is already in flight'));
                    return;
                }
                pending = { count: blobs.length, resolve, reject };
                try {
                    for (const blob of blobs) {
                        child.stdin.write(blob);
                        child.stdin.write("\n");
                    }
                    child.stdin.write("!ECHO /*-----*/\n");
                } catch (e) {
                    pending = null;
                    reject(e);
                }
            });
        },

        stop(): void {
            child.stdin.end();
        },
    };
}
