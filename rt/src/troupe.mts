import { start } from './runtimeMonitored.mjs';
import { getRuntimeObject } from './SysState.mjs';
import path  from 'path';
import yargs from 'yargs'
import fs from 'node:fs'
// let yargs = require('yargs');
// let fs = require('fs');
let p:any = yargs.argv.f;
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

