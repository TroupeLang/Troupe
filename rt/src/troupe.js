const path = require('path');
let yargs = require('yargs');
let fs = require('fs');
let runtime = require ('./runtimeMonitored.js');
let p  = yargs.argv.f;
if (!path.isAbsolute(p))  {
    p = path.normalize ( process.cwd() + "/"+  p );
}

let Top = require(p);
let top = new Top (runtime.runtime);
runtime.start (top);
