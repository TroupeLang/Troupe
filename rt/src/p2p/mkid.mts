// A small utility for creating p2p peer identifiers 

// This utility checks that the files exists in order to 
// prevent an accidental overwrite of key files

'use strict';
import * as fs from 'node:fs'
import * as fsPromises from 'node:fs/promises';
// const fsPromises = require('fs').promises
import PeerId from "peer-id";
//const args = require('yargs').argv;
import pkg from 'yargs'
const {argv} = pkg;

let outfile = argv.outfile
if (!outfile && ! (argv.privkeyfile && argv.idfile)) {
    console.log ("Troupe p2p identifier generator") 
    console.log ("Usage: node $TROUPE/rt/bulit/p2p/mkid.mjs --outfile=FILENAME | --privkeyfile=FILENAME --idfile=FILENAME [ --verbose]");
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
    if (argv.verbose) {
        console.log("Created key with id:", obj.id);      
    } 

    const s = JSON.stringify(obj);
    if (argv.outfile) {
      await ensureFileDoesntExist (argv.outfile)
      fsPromises.writeFile ((outfile as fs.PathLike), s, 'utf8');
    }


    if (argv.idfile && argv.privkeyfile) {         
      await ensureFileDoesntExist (argv.idfile) 
      await ensureFileDoesntExist (argv.privkeyfile)       
      fsPromises.writeFile ((argv.idfile as fs.PathLike), obj.id, 'utf8');
      fsPromises.writeFile ((argv.privkeyfile as fs.PathLike), obj.privKey, 'utf8' );
    }  
}) ();