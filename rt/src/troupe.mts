import { start } from './runtimeMonitored.mjs';
import { getRuntimeObject } from './SysState.mjs';
import { registerNodeNatives } from './ffi/node/index.mjs';
import path  from 'path';
import fs from 'node:fs'
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
const argv = getCliArgs();

// The node host's native modules must be registered before the first library
// link (start's linkLibs, and any linkLibs a deserialized value triggers), so
// that a "native:" link record resolves through a populated registry.
registerNodeNatives();

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
    let d = await import (p);
    let Top = d.default     
    let __userRuntime = (getRuntimeObject() as any).__userRuntime;
    let top = new Top(__userRuntime);
    start(top);

}) ()

