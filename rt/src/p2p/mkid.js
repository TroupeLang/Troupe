'use strict';

const fs = require('fs')

const args = require('yargs').argv;

let outfile = args.outfile
if (!outfile) {
    console.log ("Troupe p2p identifier generator")    
    console.log ("Usage: node $TROUPE/rt/bulit/p2p/mkid.js --outfile=FILENAME");
    process.exit(1)
}


const PeerId = require("peer-id")

PeerId.create((err,id) => {
    const obj = id.toJSON();
    if (args.verbose) {
        console.log("Created key with id:", obj.id);
    } else {
        // console.log(obj.id);
    }

    const s = JSON.stringify(obj);
    fs.writeFileSync (outfile, s, 'utf8');
})

