let p2p = require('../p2p/p2p.js')

async function foo () {
  await p2p.startp2p(null, null);
  console.log ("yay")

}


foo ()