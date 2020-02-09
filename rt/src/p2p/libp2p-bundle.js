'use strict'

const TCP = require('libp2p-tcp')
const MulticastDNS = require('libp2p-mdns')
const WS = require('libp2p-websockets')
const Bootstrap = require('libp2p-bootstrap')
const KadDHT = require('libp2p-kad-dht')

const Multiplex = require('libp2p-mplex')
const SECIO = require('libp2p-secio')
const libp2p = require('libp2p')
const defaultsDeep = require('@nodeutils/defaults-deep')
const p2pconfig = require('./p2pconfig.js')

const bootstrapers = [ // TODO: 2020-02-10: check whether this list needs updating 
                       // and ideally make this a configurable option; AA

  '/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ',
  '/ip4/104.236.176.52/tcp/4001/p2p/QmSoLnSGccFuZQJzRadHn95W2CrSFmZuTdDWP8HXaHca9z',
  '/ip4/104.236.179.241/tcp/4001/p2p/QmSoLPppuBtQSGwKDZT2M73ULpjvfd3aZ6ha4oFGL1KrGM',
  '/ip4/162.243.248.213/tcp/4001/p2p/QmSoLueR4xBeUbY9WZ9xGUUxunbKWcrNFTDAadQJmocnWm',
  '/ip4/128.199.219.111/tcp/4001/p2p/QmSoLSafTMBsPKadTEgaXctDQVcqN88CNLHXMkTNwMKPnu',
  '/ip4/104.236.76.40/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64',
  '/ip4/178.62.158.247/tcp/4001/p2p/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd',
  '/ip4/178.62.61.185/tcp/4001/p2p/QmSoLMeWqB7YGVLJN3pNLQpmmEk35v6wYtsMGLzSr5QBU3',
  '/ip4/104.236.151.122/tcp/4001/p2p/QmSoLju6m7xTh3DuokvT3886QRYqxAzb1kShaanJgW36yx'
]

class Node extends libp2p {
  constructor (_options) {    
    const defaults = {        
      connectionManager: {
        maxPeers: 100,          // this allows us to keep the memory footprint low! 2019-03-12; AA
        minPeers: 5,
        pollInterval: 2000,    
        defaultPeerValue: 1    // we later set the peer value of Troupe nodes to 1.0
      },

      modules: {
        transport: [
          TCP,
          WS
        ],
        streamMuxer: [          
          Multiplex          
        ],
        connEncryption: [
          SECIO
        ],
        peerDiscovery: [
          MulticastDNS,
          Bootstrap
        ],
        dht: KadDHT
      },
      dialer: {
        maxParallelDials: 150, // How many multiaddrs we can dial in parallel
        maxDialsPerPeer: 8, // How many multiaddrs we can dial per peer, in parallel
        dialTimeout: 10e3 // 15 second dial timeout per peer
      },
      metrics: {
        enabled: true
      },
      config: {
        peerDiscovery: {
          autoDial:true,
          mdns: {
            interval: 2e3,
            enabled: true
          },
          bootstrap: {
            interval: 1000,
            list : bootstrapers.concat(p2pconfig.relays),
            enabled: true
          }
        },
        
        dht: {
          enabled:false,
          kBucketSize: 20,                       
        },       

        relay:{
          enabled: true,
          hop :  { enabled: false, active:false }
        }   
      }
    }

    super(defaultsDeep(_options, defaults))
  }
}

module.exports = Node
