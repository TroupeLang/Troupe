// Node entry point.
//
// The command-line parser is registered before the runtime is imported: several
// runtime modules read options at import time (NodeManager, builtins/stdio and
// others), so the options record has to be available by then. That is why the
// runtime is brought in with a dynamic import rather than a static one.

import path from 'path';
import fs from 'node:fs'
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { registerNodeHost } from './hosts/node/index.mjs';
import { registerNetworkProvider } from './NetworkProvider.mjs';
import { registerCompilerService } from './CompilerService.mjs';
import { startNodeCompiler } from './hosts/node/compilerService.mjs';
import { installInterruptHandler } from './hosts/node/lifecycle.mjs';

registerNodeHost();

const argv = getCliArgs();

let p:any = argv[TroupeCliArg.File];
if (!p) {
    console.error("Error: -f/--file argument is required.");
    process.exit(1);
}
if (!path.isAbsolute(p)) {
    p = path.normalize(process.cwd() + "/" + p);
}
if (!fs.existsSync(p)) {
    console.error(`Cannot find file ${p}`);
    process.exit(1);
}
(async () => {
    // The node host's native modules must be registered before the first
    // library link (start's linkLibs, and any linkLibs a deserialized value
    // triggers), so that a "native:" link record resolves through a populated
    // registry. Their modules read options at import time, hence the dynamic
    // import after the parser is registered.
    const { registerNodeNatives } = await import('./ffi/node/index.mjs');
    registerNodeNatives();
    // The libp2p stack is registered whether or not the run is local-only,
    // as it was loaded before: a remote operation on a local-only node fails
    // inside it, as it did.
    const { p2p } = await import('./p2p/p2p.mjs');
    registerNetworkProvider(p2p);
    const rtm = await import('./runtimeMonitored.mjs');
    registerCompilerService(startNodeCompiler(() => rtm.__exitInitiated));
    const { start } = rtm;
    const { getRuntimeObject } = await import('./SysState.mjs');
    installInterruptHandler(() => getRuntimeObject().cleanup());
    let d = await import (p);
    let Top = d.default
    let __userRuntime = (getRuntimeObject() as any).__userRuntime;
    let top = new Top(__userRuntime);
    start(top);

}) ()
