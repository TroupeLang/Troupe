// What passes between a page and the runtime worker.
//
// The worker runs one program and ends. The page starts it with a run
// request and may feed input lines; the worker reports program output and
// the lifecycle messages a result socket carries on Node (ResultChannel.mts).

import { ParsedArgs } from '../../TroupeCliArgs.mjs';
import { ResultMessage } from '../../ResultChannel.mjs';

export type { MessagePortLike } from './compilerWorker.mjs';

export interface RunRequest {
    op: 'run';
    /** The compiled program: the JavaScript troupec emits for it. */
    program: string;
    /** Source texts by the path the compiler recorded, for the context an
     *  error report prints. */
    sources?: Record<string, string>;
    /** Options for the run, over the defaults; there is no command line. */
    args?: Partial<ParsedArgs>;
    /** Where compiled code comes from: a base URL for the libraries, the
     *  URL of the trp-rt service module, and the program's own modules by
     *  content hash, as a compiled project carries them (the identity a
     *  program links a module by is its hash, never a path). */
    code: { libraries: string; service: string; modules?: Record<string, { code: string; name: string }> };
    /** The node's configuration, parsed; absent entries are unconfigured. */
    config?: { aliases?: Record<string, string>; trustList?: any[]; nodeIdentity?: any };
    /** A port to a compiler worker that is serving and initialized
     *  (compilerWorker.mts). Without one the node cannot link a received
     *  closure, and reports that when one arrives. */
    compiler?: MessagePort;
    /** Whether the page consumes the structured result (ResultChannel's
     *  enabled): when it does, the console copy of the main thread's
     *  result is not printed. Default true. */
    supervised?: boolean;
    /** The name of the OPFS directory holding persisted values. Default
     *  'troupe'. */
    storage?: string;
}

export type PageMessage =
    | RunRequest
    | { op: 'stdin'; line: string };

export type WorkerMessage =
    | ResultMessage
    | { type: 'stdout'; text: string }
    | { type: 'stderr'; text: string };
