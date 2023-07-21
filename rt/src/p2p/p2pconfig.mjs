'use strict'
const P2PCONFIG_FILE = 'p2pconfig.json'
let logger;
(async() => {
    let { mkLogger } = await import ('../logger.mjs');
    logger = mkLogger ('p2p-config','info');
})()

import { existsSync, readFileSync } from 'fs';
let relays


// TODO: change the relay address to be the actual address
let default_relays =
  // ["/dns4/relay.troupe-lang.net/tcp/5555/p2p/QmcQpBNGULxRC3QmvxVGXSw8BarpMvdADYvFtmvKAL5QMe"]
  // TODO: dns resolution of the relay has stopped working
  ["/ip4/134.209.92.133/tcp/5555/ws/p2p/12D3KooWShh9qmeS1UEgwWpjAsrjsigu8UGh8DRKyx1UG6HeHzjf"]
  
let known_nodes = [
    {nodeid:"QmXfj4ysaS4pARJU5uUP59B47aCQP6X6FH6cm5otLhcMPa", ip: "/ip4/134.209.90.7/tcp/6789"},


    {nodeid:"QmNUiTnU1J5rNFtGXUfZJzSmx2GahSFCPjeSfSTbGoAR4q", ip: "/ip4/142.93.235.197/tcp/6789"},
    {nodeid:"QmW3oruhRQEuZXCtFWUEwE6DT1WyvifcR6YYtBEK6QTsMo", ip: "/ip4/167.71.68.246/tcp/6789"},
    {nodeid:"QmYHExHtTzFEjdwyQcJkD9gaKKgfwvGswoFb4simrJn5Q6", ip: "/ip4/188.166.69.210/tcp/6789"},
    {nodeid:"QmcRfB3SJp92t7GMgS5rygXuSDPm7q31GeAdP8q9HutyYT", ip: "/ip4/128.199.61.30/tcp/6789"},
    {nodeid:"QmSyj7FUykAhog46qDekYKjWDF4Y4PhPbP2hDkEmwz679b", ip: "/ip4/142.93.234.253/tcp/6789"},
    {nodeid:"QmS9tkoqKEPrfgsGMhqQgNT1pcZjCq1XRonUiR9uXEoj9e", ip: "/ip4/188.166.70.113/tcp/6789"},
    {nodeid:"QmYVDakQNvBhVCHeu1JCHJYc2sG1ESBwC3PB1vNzAFJWbH", ip: "/ip4/128.199.41.250/tcp/6789"},
    {nodeid:"QmX2edVZhWVa9Q6gbpwNZJGXqeLgcAvAYTga2m25dwWudH", ip: "/ip4/188.166.70.132/tcp/6789"},
    {nodeid:"QmbkpnNgD8uu9FArPPyzYUYuuVZtCo7n3jQ5Rz2nqiZEWD", ip: "/ip4/167.71.76.8/tcp/6789"},
    {nodeid:"QmYFdcq31Gnch87kkqFjWt5R1GH8jTPNspz5XxBzC6hJ1r", ip: "/ip4/104.248.86.46/tcp/6789"}
]
  

if (existsSync(P2PCONFIG_FILE)) {
  try {
    let s = readFileSync(P2PCONFIG_FILE) 
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

export default { relays, known_nodes }
