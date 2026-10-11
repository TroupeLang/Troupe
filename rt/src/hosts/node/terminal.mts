// Node's terminal: the process's standard streams, with readline delivering
// input lines.

import * as rl from 'node:readline';
import { Terminal } from '../../Terminal.mjs';

/** Node's readline interface, created on first use: on a terminal, creating
 * it puts stdin into raw mode with readline's own echo. `close()` resets the
 * mode and releases the stream, and is irreversible for an interface
 * object, which is why a later read builds a fresh one. */
let readline: rl.Interface | null = null;

export const nodeTerminal: Terminal = {
    stdin: process.stdin,
    stdout: process.stdout,
    stderr: process.stderr,

    readLines(listener: (line: string) => void): void {
        if (readline === null) {
            readline = rl.createInterface({
                input: process.stdin,
                output: process.stdout
            });
            readline.on('line', listener);
        }
    },

    closeInput(): void {
        if (readline !== null) {
            readline.close();
            readline = null;
        }
    },
};
