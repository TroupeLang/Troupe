// A small utility for creating p2p peer identifiers 

// This utility checks that the files exists in order to 
// prevent an accidental overwrite of key files

'use strict';
import * as fs from 'node:fs'
import * as fsPromises from 'node:fs/promises';
import { keys } from '@libp2p/crypto'
import { peerIdFromPrivateKey } from '@libp2p/peer-id'
import { toString as uint8ArrayToString } from 'uint8arrays/to-string'
import yargs from 'yargs'
import { hideBin } from 'yargs/helpers';
const argv:any = yargs(hideBin(process.argv)).parse()


let outfile = argv.outfile
if (!outfile && ! (argv.privkeyfile && argv.idfile)) {
    console.log ("Troupe p2p identifier generator") 
    console.log ("Usage: node $TROUPE/p2p-tools/built/mkid.mjs --outfile=FILENAME | --privkeyfile=FILENAME --idfile=FILENAME [ --verbose]");
    process.exit(1)
}

async function ensureFileDoesntExist (f) {
  if (fs.existsSync (f)) {
    console.error ("File already exists", f)
    process.exit (1)
 }
}

(async () => {
    const privateKey = await keys.generateKeyPair('Ed25519');
    const peerid = await peerIdFromPrivateKey(privateKey);
    const privKeyBytes = keys.privateKeyToProtobuf(privateKey);
    const publicKey = privateKey.publicKey;
    const pubKeyBytes = keys.publicKeyToProtobuf(publicKey);
    
    const obj = {
      id : peerid.toString(),
      privKey : uint8ArrayToString(privKeyBytes, 'base64pad'),
      pubKey : uint8ArrayToString(pubKeyBytes, 'base64pad')
    };
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