'use strict'
const P2PCONFIG_FILE = 'p2pconfig.json'
const logger = require('../logger.js').mkLogger('p2p','info');
const fs = require('fs');
let relays

let default_relays =
  ["/dns4/troupe-lbs-primary.askarov.net/tcp/5555/p2p/QmUjm46PZGcN7KwkjzcsTFpccWZTAVKijLEgCS9XBKzAoD"]
  

if (fs.existsSync(P2PCONFIG_FILE)) {
  try {
    let s = fs.readFileSync(P2PCONFIG_FILE) 
    let o = JSON.parse (s);
    console.log (o.relays)
    if (o.relays) {      
      relays = o.relays
    } else {
      throw new Error ("relays field undefined")
    }
  } catch (err) {
    logger.error ("error parsing p2p configuration file")    
    relays = default_relays
  }
} else {
  relays = default_relays
}

module.exports = { relays }
