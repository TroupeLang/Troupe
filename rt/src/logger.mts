// The runtime's loggers. A host registers a logger factory before the
// runtime is imported, since several modules create their loggers at import
// time; the Node host registers winston (hosts/node/logger.mts). When no
// factory is registered, mkLogger gives a console logger: one line per
// message, `timestamp [label] level: message`, the level coloured when
// colour is on, written to the host's console. The browser runs on that
// default, since winston does not load there, and so do tools and tests
// that import the core without a host. Levels are the npm set winston uses,
// so a logger created at 'info' prints info, warn and error and not debug.

import { isColorEnabled } from './colorConfig.mjs';
import { getHostConsole } from './hostConsole.mjs';

export interface Logger {
    error(message: any): void;
    warn(message: any): void;
    info(message: any): void;
    debug(message: any): void;
    isLevelEnabled(level: string): boolean;
}

export type LoggerFactory = (label: string, level: string) => Logger;

let __factory: LoggerFactory | null = null;

export function registerLoggerFactory(f: LoggerFactory): void {
    __factory = f;
}

const LEVELS: Record<string, number> = {
    error: 0, warn: 1, info: 2, http: 3, verbose: 4, debug: 5, silly: 6,
};

// ANSI colours per level, as winston's colorize format renders them.
const COLORS: Record<string, [string, string]> = {
    error: ['\x1b[31m', '\x1b[39m'],
    warn: ['\x1b[33m', '\x1b[39m'],
    info: ['\x1b[32m', '\x1b[39m'],
    http: ['\x1b[32m', '\x1b[39m'],
    verbose: ['\x1b[36m', '\x1b[39m'],
    debug: ['\x1b[34m', '\x1b[39m'],
    silly: ['\x1b[35m', '\x1b[39m'],
};

/** The default logger: the host's console, in winston's line format. */
export class ConsoleLogger implements Logger {
    private readonly threshold: number;

    constructor(private readonly label: string, level: string) {
        this.threshold = LEVELS[level] ?? LEVELS.info;
    }

    isLevelEnabled(level: string): boolean {
        return (LEVELS[level] ?? LEVELS.info) <= this.threshold;
    }

    log(level: string, message: any): void {
        if (!this.isLevelEnabled(level)) return;
        const shown = isColorEnabled()
            ? COLORS[level][0] + level + COLORS[level][1]
            : level;
        getHostConsole().log(
            `${new Date().toISOString()} [${this.label}] ${shown}: ${message}`);
    }

    error(message: any): void { this.log('error', message); }
    warn(message: any): void { this.log('warn', message); }
    info(message: any): void { this.log('info', message); }
    debug(message: any): void { this.log('debug', message); }
}

export function mkLogger(l: string, level = 'info'): Logger {
    return __factory ? __factory(l, level) : new ConsoleLogger(l, level);
}

/** A lazy debug logger for hot paths, used as a tagged template literal:
 *
 *      const debug = mkDebugTag(logger);
 *      debug `delivering ${message} at ${pc}`
 *
 *  When debug logging is off the tag returns before the message string is
 *  built, so interpolated values are never stringified — unlike an ordinary
 *  template literal argument, which is fully constructed before the logger
 *  can discard it. Values with a stringRep() method (runtime values,
 *  levels, pids) are rendered with it; everything else with String().
 *
 *  The enabled check is captured at creation time: log levels in this
 *  runtime are fixed at startup from CLI flags.
 */
export function mkDebugTag (logger: Logger) {
  const enabled = logger.isLevelEnabled('debug');
  return (strings: TemplateStringsArray | any, ...vals: any[]) => {
    if (!enabled) return;
    if (!Array.isArray(strings)) {
      // called as a plain function, debug(msg) — kept for migration
      logger.debug(strings);
      return;
    }
    let s = strings[0];
    for (let i = 0; i < vals.length; i++) {
      const v = vals[i];
      s += (v != null && typeof v.stringRep === 'function' ? v.stringRep() : String(v))
           + strings[i + 1];
    }
    logger.debug(s);
  };
}
