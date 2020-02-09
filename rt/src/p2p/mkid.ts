// A small utility for creating p2p peer identifiers 

// This utility checks that the files exists in order to 
// prevent an accidental overwrite of key files

'use strict';
const fs = require('fs')
const fsPromises = require('fs').promises
const PeerId = require("peer-id")
const args = require('yargs').argv;

let outfile = args.outfile
if (!outfile && ! (args.privkeyfile && args.idfile)) {
    console.log ("Troupe p2p identifier generator") 
    console.log ("Usage: node $TROUPE/rt/bulit/p2p/mkid.js --outfile=FILENAME | --privkeyfile=FILENAME --idfile=FILENAME [ --verbose]");
    process.exit(1)
}

async function ensureFileDoesntExist (f) {
  if (fs.existsSync (f)) {
    console.error ("File already exists", f)
    process.exit (1)
 }
}

(async () => {
    let id = await PeerId.create();
    const obj = id.toJSON();
    if (args.verbose) {
        console.log("Created key with id:", obj.id);      
    } 

    const s = JSON.stringify(obj);
    if (args.outfile) {
      await ensureFileDoesntExist (args.outfile)
      fsPromises.writeFile (outfile, s, 'utf8');
    }


    if (args.idfile && args.privkeyfile) {         
      await ensureFileDoesntExist (args.idfile) 
      await ensureFileDoesntExist (args.privkeyfile)       
      fsPromises.writeFile (args.idfile, obj.id, 'utf8');
      fsPromises.writeFile (args.privkeyfile, obj.privKey, 'utf8' );
    }  
}) ();