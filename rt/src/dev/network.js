let p2p = require('../p2p/p2p.js');
async function main() {
    await p2p.startp2p(null, null);
    console.log("yay");
}
main();
