import { Thread } from "./Thread.mjs";
// import colors = require('colors/safe');
import chalk from 'chalk'
import { SchedulerInterface } from "./SchedulerInterface.mjs";
import { configureColors } from './colorConfig.mjs';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { readFileSync, statSync } from 'fs';
import { resolve, isAbsolute } from 'path';

// Ensure colors are configured when this module is loaded
configureColors();

// ============================================================================
// Source Code Display Functions
// These functions provide visual error context similar to compiler errors
// ============================================================================

/** Tab stop width for display (matches common editor defaults) */
const TAB_WIDTH = 8;

/**
 * Expand tabs to spaces for consistent display.
 * Uses 8-space tab stops (matching ParseError.hs behavior).
 */
function expandTabs(line: string): string {
    let result = '';
    let col = 0;
    for (const ch of line) {
        if (ch === '\t') {
            const spaces = TAB_WIDTH - (col % TAB_WIDTH);
            result += ' '.repeat(spaces);
            col += spaces;
        } else {
            result += ch;
            col++;
        }
    }
    return result;
}

/**
 * Adjust column number for tabs in the source line.
 * Converts a 1-indexed column in the original (with tabs) to
 * the equivalent position after tab expansion.
 */
function adjustForTabs(line: string, col: number): number {
    let displayCol = 0;
    for (let i = 0; i < col - 1 && i < line.length; i++) {
        if (line[i] === '\t') {
            displayCol += TAB_WIDTH - (displayCol % TAB_WIDTH);
        } else {
            displayCol++;
        }
    }
    return displayCol + 1; // Return 1-indexed
}

/**
 * Create a caret line pointing to error column (1-indexed).
 */
function makeCaretLine(col: number): string {
    return ' '.repeat(col - 1) + '^';
}

/**
 * Parse a source location string into components.
 * Expected format: "filepath:line:col"
 */
function parseSourceLocation(loc: string): { filePath: string; line: number; col: number } | null {
    // Match path (may contain colons on Windows), then :line:col
    const match = loc.match(/^(.+):(\d+):(\d+)$/);
    if (!match) return null;
    return {
        filePath: match[1],
        line: parseInt(match[2], 10),
        col: parseInt(match[3], 10)
    };
}

/**
 * Check if a path points to a regular file (not a device, pipe, socket, etc.).
 * Returns false for special files like /dev/stdin that can't be re-read.
 */
function isRegularFile(path: string): boolean {
    try {
        const stat = statSync(path);
        return stat.isFile();
    } catch {
        return false;
    }
}

/**
 * Resolve a potentially relative file path to an absolute path.
 * Only returns paths to regular files (excludes devices, pipes, sockets).
 */
function resolveSourcePath(filePath: string): string | null {
    // If already absolute, check if it's a regular file
    if (isAbsolute(filePath)) {
        if (isRegularFile(filePath)) return filePath;
        return null;
    }

    // Try relative to current working directory
    const cwdPath = resolve(process.cwd(), filePath);
    if (isRegularFile(cwdPath)) return cwdPath;

    return null;
}

/**
 * Attempt to read a source line from a file.
 * Returns null if file is unavailable or line is out of range.
 */
function getSourceLine(filePath: string, lineNum: number): string | null {
    const resolvedPath = resolveSourcePath(filePath);
    if (!resolvedPath) return null;

    try {
        const content = readFileSync(resolvedPath, 'utf-8');
        const lines = content.split('\n');
        if (lineNum > 0 && lineNum <= lines.length) {
            return lines[lineNum - 1];
        }
    } catch {
        // File read error - silently return null
    }
    return null;
}

/**
 * Result of attempting to format source context.
 */
interface SourceContextResult {
    /** Whether source was successfully read */
    available: boolean;
    /** Lines to display (source line and caret, or unavailable message) */
    lines: string[];
}

/**
 * Format source context for an error location.
 * Returns the source line with line number prefix and caret line,
 * or an unavailable message if source cannot be read.
 */
function formatSourceContext(sourceLocation: string): SourceContextResult {
    const parsed = parseSourceLocation(sourceLocation);
    if (!parsed) {
        return { available: false, lines: ['  (source file not available)'] };
    }

    const { filePath, line, col } = parsed;
    const sourceLine = getSourceLine(filePath, line);

    if (sourceLine === null) {
        return { available: false, lines: ['  (source file not available)'] };
    }

    // Format like compiler: "  N | source code"
    const lineNumStr = String(line);
    const lineNumWidth = lineNumStr.length;
    const expandedLine = expandTabs(sourceLine);
    const adjustedCol = adjustForTabs(sourceLine, col);

    const sourceDisplay = `  ${lineNumStr} | ${expandedLine}`;
    const caretDisplay = `  ${' '.repeat(lineNumWidth)} | ${makeCaretLine(adjustedCol)}`;

    return {
        available: true,
        lines: ['', sourceDisplay, caretDisplay, '']
    };
}

/**
 * Classification of runtime error kinds.
 * A semantic taxonomy, declared at every raise site: it records WHY a thread
 * error exists, independently of how it is reported. In particular, IFCCheck
 * marks a verdict of the security monitor — and on the HandlerError path
 * (errors inside receive patterns, guards, and sandboxes, which resume the
 * trapper instead of stopping the thread) the kind is the hook for the open
 * policy question of whether monitor verdicts should be recoverable at all.
 * Do not remove on the grounds that nothing reads it; the declaration is the
 * point.
 */
export enum ErrorKind {
    /** Type mismatch in built-in function arguments (e.g., passing string to numeric operation) */
    BuiltInArgsTypeMismatch,
    /** Information flow control violation (e.g., declassification without authority) */
    IFCCheck,
    /** Dynamic type error in user code (e.g., pattern match failure) */
    DynTypeError
}

export abstract class TroupeError extends Error {
    abstract handleError (sched: SchedulerInterface) : void
}

export abstract class ThreadError extends TroupeError {
    abstract errorMessage: string
    thread: Thread
    constructor (thread:Thread) {
        super ()
        this.thread = thread;
    }
}

export abstract class StopThreadError extends ThreadError {
    abstract explainstr: string;
    abstract errorKind: ErrorKind;
    handleError (sched) {
        let console = this.thread.rtObj.xconsole

        // The reported position is the machine's own position state: the source
        // position of the responsible user-level call (set on tail calls and on
        // failing user-code assertions). It is never derived from the host JS
        // stack. Where the machine has no position, none is printed.
        let sourceLocation: string | null = this.thread.lastCallSourcePos;

        // Format error with source context (visually consistent with compiler errors)
        console.log(chalk.red("Runtime error in thread " + this.thread.tidErrorStringRep()));

        // Indicate if error occurred in restored code (deserialized closure)
        if (this.thread.currentSourceMap?.__isDynamic) {
            console.log(chalk.yellow(">> (in dynamically loaded code)"));
        }

        // Show source context if location is available
        if (sourceLocation) {
            const sourceContext = formatSourceContext(sourceLocation);
            for (const line of sourceContext.lines) {
                console.log(chalk.red(line));
            }
        }

        console.log(chalk.red(">> " + this.errorMessage));

        if (sourceLocation) {
            console.log(chalk.red(">> at " + sourceLocation));
        }

        if (getCliArgs()[TroupeCliArg.Explain] && this.explainstr) {
            console.log(chalk.yellow(this.explainstr));
        }
        sched.stopThreadWithErrorMessage(this.thread, this.errorMessage);
    }
}

export class StrThreadError extends StopThreadError {
    errstr: string;
    explainstr : string;
    errorKind: ErrorKind;
    get errorMessage () { return this.errstr }
    constructor (thread:Thread, errstr:string, explainstr: string, errorKind: ErrorKind = ErrorKind.DynTypeError) {
        super (thread) ;
        this.errstr = errstr;
        this.explainstr = explainstr;
        this.errorKind = errorKind;
    }
}

export class HandlerError extends ThreadError {
    errstr: string
    errorKind: ErrorKind
    constructor (thread: Thread, errstr: string, errorKind: ErrorKind = ErrorKind.DynTypeError) {
        super (thread);
        this.errstr = errstr;
        this.errorKind = errorKind;
    }
    get errorMessage () { return this.errstr }
    handleError( sched:SchedulerInterface  ) {
          // we have an error inside of an receive pattern or guard;
          // we are discarding the rest of the current thread and are
          // scheduling the execution of the handler
          let console = this.thread.rtObj.xconsole
          console.log (chalk.yellow (`Warning: runtime exception in the handler or sandbox: ${this.errstr}`))
          this.thread.next = this.thread.handlerState.getTrapper();
          sched.scheduleThread(this.thread)
    }
}

export class ImplementationError extends Error { // observe that this does not inherit from TroupeError
    errstr :string
    constructor (s: string) {
        super ()
        this.errstr = s 
    }
}