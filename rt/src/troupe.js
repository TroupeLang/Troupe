import {start } from './runtimeMonitored.js'
import { getRuntimeObject } from './SysState.js';
const path = require('path');
let yargs = require('yargs');
let fs = require('fs');
let p  = yargs.argv.f;
if (!path.isAbsolute(p))  {
    p = path.normalize ( process.cwd() + "/"+  p );
}

if (!fs.existsSync(p)) {
    console.error (`Cannot find file ${p}`)
    process.exit (1)
}

let Top = require(p);

let __userRuntime = getRuntimeObject().__userRuntime
let top = new Top (__userRuntime);
start (top);
