// The browser host: every registration the runtime needs in place before it
// is imported, from what the page said in its run request.

import { setCliArgs, defaultCliArgs, ParsedArgs } from '../../TroupeCliArgs.mjs';
import { registerCodeStore } from '../../CodeStore.mjs';
import { registerConfigStore } from '../../ConfigStore.mjs';
import { registerPersistStore } from '../../PersistStore.mjs';
import { registerResultChannel } from '../../ResultChannel.mjs';
import { ResultMessage } from '../../ResultChannel.mjs';
import { registerTerminal, Terminal } from '../../Terminal.mjs';
import { registerHostConsole } from '../../hostConsole.mjs';
import { registerLifecycle } from '../../Lifecycle.mjs';
import { registerClock } from '../../Clock.mjs';
import { registerSignals } from '../../Signals.mjs';
import { registerCompilerService, CompilerService } from '../../CompilerService.mjs';
import { browserCodeStore, BrowserCodeSources } from './codeStore.mjs';
import { browserConfigStore, BrowserConfig } from './configStore.mjs';
import { opfsPersistStore, hasOriginPrivateFileSystem } from './persistStore.mjs';
import { browserResultChannel } from './resultChannel.mjs';
import { browserConsole, installConsole } from './console.mjs';
import { browserLifecycle } from './lifecycle.mjs';
import { browserClock } from './clock.mjs';
import { browserSignals } from './signals.mjs';
import { browserCompilerService } from './compilerService.mjs';
import { MessagePortLike } from './compilerWorker.mjs';

export interface BrowserHostOptions {
    args: Partial<ParsedArgs>;
    terminal: Terminal;
    code: BrowserCodeSources;
    config: BrowserConfig;
    /** Where lifecycle messages go. */
    post: (msg: ResultMessage) => void;
    /** Ends the worker. */
    close: (code: number) => void;
    supervised: boolean;
    /** The OPFS directory for persisted values. */
    storage: string;
    /** A port to a serving, initialized compiler worker, or none. */
    compiler: MessagePortLike | null;
}

/** Options for a run in the browser: the defaults, then what the page said. */
export function browserCliArgs(overrides: Partial<ParsedArgs> = {}): ParsedArgs {
    return { ...defaultCliArgs(), ...overrides };
}

/** A node with no compiler: a received closure is reported, not linked. */
const noCompiler: CompilerService = {
    async linkIR() {
        throw new Error('this node has no compiler: a received closure cannot be linked');
    },
    stop() {},
};

export function registerBrowserHost(o: BrowserHostOptions): void {
    setCliArgs(browserCliArgs(o.args));
    registerTerminal(o.terminal);
    const console = browserConsole(o.terminal);
    installConsole(console);
    registerHostConsole(console);
    registerResultChannel(browserResultChannel(o.post, o.supervised, o.close));
    registerCodeStore(browserCodeStore(o.code));
    registerConfigStore(browserConfigStore(o.config));
    // Without the origin private file system -- a host that is not a
    // browser running this code -- nothing is registered, and a save
    // reports that no store exists.
    if (hasOriginPrivateFileSystem()) {
        registerPersistStore(opfsPersistStore(o.storage));
    }
    registerLifecycle(browserLifecycle(o.close));
    registerClock(browserClock);
    registerSignals(browserSignals);
    registerCompilerService(o.compiler ? browserCompilerService(o.compiler) : noCompiler);
    // No logger factory is registered: the runtime's default logger writes
    // to the host console above, and winston does not load in a browser.
    // No compression is registered: the browser's gzip is asynchronous and
    // the codec builtins are not (builtins/codec.mts), so they report its
    // absence as a thread error.
}
