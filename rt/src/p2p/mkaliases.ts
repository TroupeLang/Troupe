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




import * as fs from 'node:fs';
import * as path from 'path';
import yargs from 'yargs'

const argv:any =yargs.argv

const files_args:any = argv['include'];

const files = Array.isArray (files_args) ? files_args : [files_args] 
const outfile=argv['outfile']

let aliases = {}
let trustmap = []


for (let i = 0; i < files.length; i ++) {
  let fname = files[i]
  if (typeof(fname) !='string')  {
    continue;
  }
  let json = JSON.parse (fs.readFileSync(fname,'utf-8').toString());
  let alias = path.basename(fname, ".json")
  aliases[alias] = json.id   
  if (argv.trustmap) {
    let level; 
    if (argv.droptrustprefix) {
      let j = alias.indexOf(argv.droptrustprefix) ;
      if (j == 0) {
        level = alias.substr (argv.droptrustprefix.length)
      } else {
        level = alias;
      }      
    }
    trustmap.push ( {id:json.id, level:level})
  }
}

fs.writeFileSync (outfile, JSON.stringify(aliases), 'utf8');
if (argv.trustmap) {
  fs.writeFileSync (argv.trustmap, JSON.stringify (trustmap), 'utf8')
}

