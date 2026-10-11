// The terminal a node talks to: its three standard descriptors and the
// input lines they deliver. A program names a descriptor as a value, and
// the channel checks sit on the operations (builtins/stdio.mts); the host
// supplies the streams behind the descriptors.

export interface OutputStream {
    write(text: string): void;
}

export interface Terminal {
    /** The identity the input descriptor's value carries. */
    readonly stdin: unknown;
    readonly stdout: OutputStream;
    readonly stderr: OutputStream;

    /**
     * Starts delivering input lines to the listener. The first call opens
     * the input source; later calls while it is open do nothing. On a
     * terminal, opening the source takes the line discipline over, so a
     * program that never reads a line never opens it.
     */
    readLines(listener: (line: string) => void): void;

    /** Closes the input source. A later readLines opens it again. */
    closeInput(): void;
}

let __terminal: Terminal | null = null;

export function registerTerminal(terminal: Terminal): void {
    __terminal = terminal;
}

export function getTerminal(): Terminal {
    if (__terminal == null) {
        throw new Error(
            'no terminal registered: the host must call registerTerminal() '
            + 'before the runtime is imported');
    }
    return __terminal;
}
