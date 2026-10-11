'use strict'
const P2PCONFIG_FILE = 'p2pconfig.json'
let logger;
(async() => {
    let { mkLogger } = await import ('../logger.mjs');
    logger = mkLogger ('p2p-config','info');
})()

import { existsSync, readFileSync } from 'fs';
import { resolve } from 'path';
import { getTroupeRoot } from '../hosts/node/troupeRoot.mjs';
let relays

let default_relays = []
  
let known_nodes = [
    {nodeid:"QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN", ip: "/dnsaddr/bootstrap.libp2p.io"},
    {nodeid:"QmQCU2EcMqAqQPR2i9bChDtGNJchTbq5TbXJJ16u19uLTa", ip: "/dnsaddr/bootstrap.libp2p.io"},
    {nodeid:"QmbLHAnMoJPWSCR5Zhtx6BHJX9KiKNN6tpvbUcqanj75Nb", ip: "/dnsaddr/bootstrap.libp2p.io"},
    {nodeid:"QmcZf59bWwK5XFi76CZX8cbJ4BhTzzA3gU1ZjYZcYW3dwt", ip: "/dnsaddr/bootstrap.libp2p.io"},
    {nodeid:"QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ", ip: "/ip4/104.131.131.82/tcp/4001"}
]
  

// Load relays from a config file, returns null if not found or invalid
function loadRelaysFromConfig(path) {
  if (!existsSync(path)) return null;
  try {
    let o = JSON.parse(readFileSync(path));
    return o.relays || null;
  } catch (err) {
    logger?.error(`error parsing p2p configuration file: ${path}`)
    return null;
  }
}

// Precedence: local p2pconfig.json > $TROUPE/p2pconfig.json > default
relays = loadRelaysFromConfig(P2PCONFIG_FILE)
      || loadRelaysFromConfig(resolve(getTroupeRoot(), P2PCONFIG_FILE))
      || default_relays;

let cliRelays = null;

export function setCliRelays(relayAddresses) {
  if (relayAddresses && relayAddresses.length > 0) {
    cliRelays = relayAddresses;
    logger?.info(`Using CLI-provided relay addresses: ${relayAddresses.join(', ')}`);
  }
}

export function getRelays() {
  // CLI relays take precedence
  if (cliRelays && cliRelays.length > 0) {
    return cliRelays;
  }
  // Otherwise use file-based or default relays
  return relays;
}

export default { relays: getRelays(), known_nodes, setCliRelays, getRelays }
