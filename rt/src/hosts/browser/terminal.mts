// The browser's terminal: the two output sinks post their text to the page,
// and input lines come from the page.
//
// The runtime compares descriptors by identity against these three objects
// (builtins/stdio.mts), so they are created once per worker. None is a
// terminal: there is no line discipline to take over and no size to report.

import { Terminal, OutputStream } from '../../Terminal.mjs';

export interface BrowserTerminal {
    terminal: Terminal;
    /** Delivers one input line. Lines arriving while no reader is open are
     *  kept, as a pipe keeps them, until the program opens its input. */
    feedLine(line: string): void;
}

interface Sink extends OutputStream {
    readonly isTTY: false;
}

export function browserTerminal(post: (kind: 'stdout' | 'stderr', text: string) => void): BrowserTerminal {
    const stdout: Sink = { isTTY: false, write: (text) => post('stdout', text) };
    const stderr: Sink = { isTTY: false, write: (text) => post('stderr', text) };
    const stdin = { isTTY: false };

    let listener: ((line: string) => void) | null = null;
    const pending: string[] = [];

    const terminal: Terminal = {
        stdin,
        stdout,
        stderr,
        readLines(l) {
            if (listener !== null) return;
            listener = l;
            while (pending.length > 0 && listener !== null) {
                listener(pending.shift()!);
            }
        },
        closeInput() {
            listener = null;
        },
    };

    return {
        terminal,
        feedLine(line) {
            if (listener !== null) {
                listener(line);
            } else {
                pending.push(line);
            }
        },
    };
}
