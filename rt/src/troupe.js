const path = require('path');
let yargs = require('yargs');
let fs = require('fs');
let runtime = require ('./runtimeMonitored.js');
let p  = yargs.argv.f;
if (!path.isAbsolute(p))  {
    p = path.normalize ( process.cwd() + "/"+  p );
}

if (!fs.existsSync(p)) {
    console.error (`Cannot find file ${p}`)
    process.exit (1)
}

let Top = require(p);

let top = new Top (runtime.runtime);
runtime.start (top);
