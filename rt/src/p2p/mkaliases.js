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
let trustmap = []


for (let i in files) {
  let fname = files[i]
  let json = JSON.parse (fs.readFileSync(fname));
  let alias = path.basename(fname, ".json")
  aliases[alias] = json.id   
  if (argv.trustmap) {
    let level; 
    
    if (argv.droptrustprefix) {
      let j = alias.indexOf(argv.droptrustprefix) ;
      console.log (j, argv.droptrustprefix)
      if (j == 0) {
        level = alias.substr (argv.droptrustprefix.length)
      } else {
        level = alias;
      }
      console.log (level)
    }
    trustmap.push ( {id:json.id, level:level})

  }
}

fs.writeFileSync (outfile, JSON.stringify(aliases), 'utf8');
if (argv.trustmap) {
  fs.writeFileSync (argv.trustmap, JSON.stringify (trustmap), 'utf8')
}

