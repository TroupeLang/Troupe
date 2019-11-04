'use strict';

const fs = require('fs')

const args = require('yargs').argv;

let outfile = args.outfile
if (!outfile) {
    console.log ("outfile option required");
    process.exit(1)
}


const PeerId = require("peer-id")

PeerId.create((err,id) => {
    const obj = id.toJSON();
    console.log("Created key with id:", obj.id);

    const s = JSON.stringify(obj);
    fs.writeFileSync (outfile, s, 'utf8');
})

