/***
 * mkaliases.js 
 * 
 * A helper script for generating aliases from a set of given 
 * files.
 * 
 * 
 * Author: Aslan Askarov, aslan@askarov.net 
 * 
 */


'use strict'

const fs=require('fs');
const path=require('path');
const argv=require('yargs').argv
const files=argv['include'];
const outfile=argv['outfile']

let aliases = {}

for (let i in files) {
  let fname = files[i]
  let json = JSON.parse (fs.readFileSync(fname));
  let alias = path.basename(fname, ".json")
  aliases[alias] = json.id   
}

fs.writeFileSync (outfile, JSON.stringify(aliases), 'utf8');


