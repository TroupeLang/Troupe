'use strict'



const TCP = require ('libp2p-tcp')
const  MulticastDNS = require ( 'libp2p-mdns')
const WS = require ( 'libp2p-websockets')
const  Bootstrap = require ( 'libp2p-bootstrap')
const  KadDHT = require ( 'libp2p-kad-dht')
const  Multiplex = require ( 'libp2p-mplex')
const SECIO = require( 'libp2p-secio')
const Libp2p = require('libp2p')
import fs from 'fs'
import yargs from 'yargs'
const PeerId = require("peer-id")
const PeerInfo = require ("peer-info")
const AggregateError = require('aggregate-error');
const pipe = require('it-pipe')



let logLevel = 'debug'

const logger = require('../logger.js').mkLogger('p2p',logLevel);
const info = (x: any) => logger.info(x)
const debug = (x: string) => logger.debug(x)
const error = (x: string) => logger.error(x);
const multiaddr = require('multiaddr')
const _PROTOCOL = "/TroupeRelay/"
const _PORT = 5555




let _lastTick:any = new Date ()
let _tickCounter = 0;
function healthMonitor ()  {
  let now:any = new Date ()
  debug (`>> Tick counter ${_tickCounter ++} ${now - _lastTick}`);
  _lastTick = now;
  setTimeout (healthMonitor, 1000)
}

healthMonitor()

const bootstrapers = [  
    '/ip4/104.236.176.52/tcp/4001/ipfs/QmSoLnSGccFuZQJzRadHn95W2CrSFmZuTdDWP8HXaHca9z',
    '/ip4/104.131.131.82/tcp/4001/ipfs/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ',
    '/ip4/104.236.179.241/tcp/4001/ipfs/QmSoLPppuBtQSGwKDZT2M73ULpjvfd3aZ6ha4oFGL1KrGM',
    '/ip4/162.243.248.213/tcp/4001/ipfs/QmSoLueR4xBeUbY9WZ9xGUUxunbKWcrNFTDAadQJmocnWm',
    '/ip4/128.199.219.111/tcp/4001/ipfs/QmSoLSafTMBsPKadTEgaXctDQVcqN88CNLHXMkTNwMKPnu',
    '/ip4/104.236.76.40/tcp/4001/ipfs/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64',
    '/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd',
    '/ip4/178.62.61.185/tcp/4001/ipfs/QmSoLMeWqB7YGVLJN3pNLQpmmEk35v6wYtsMGLzSr5QBU3',
//     // '/ip4/104.236.151.122/tcp/4001/ipfs/QmSoLju6m7xTh3DuokvT3886QRYqxAzb1kShaanJgW36yx',
//     '/ip6/2604:a880:1:20::1f9:9001/tcp/4001/ipfs/QmSoLnSGccFuZQJzRadHn95W2CrSFmZuTdDWP8HXaHca9z',
//     '/ip6/2604:a880:1:20::203:d001/tcp/4001/ipfs/QmSoLPppuBtQSGwKDZT2M73ULpjvfd3aZ6ha4oFGL1KrGM',
//     '/ip6/2604:a880:0:1010::23:d001/tcp/4001/ipfs/QmSoLueR4xBeUbY9WZ9xGUUxunbKWcrNFTDAadQJmocnWm',
//     '/ip6/2400:6180:0:d0::151:6001/tcp/4001/ipfs/QmSoLSafTMBsPKadTEgaXctDQVcqN88CNLHXMkTNwMKPnu',
//     '/ip6/2604:a880:800:10::4a:5001/tcp/4001/ipfs/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64',
//     '/ip6/2a03:b0c0:0:1010::23:1001/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd',
//     '/ip6/2a03:b0c0:1:d0::e7:1/tcp/4001/ipfs/QmSoLMeWqB7YGVLJN3pNLQpmmEk35v6wYtsMGLzSr5QBU3',
//     // '/ip6/2604:a880:1:20::1d9:6001/tcp/4001/ipfs/QmSoLju6m7xTh3DuokvT3886QRYqxAzb1kShaanJgW36yx',
//     '/dns4/wss0.bootstrap.libp2p.io/tcp/443/wss/ipfs/QmZMxNdpMkewiVZLMRxaNxUeZpDUb34pWjZ1kZvsd16Zic',
//     '/dns4/wss1.bootstrap.libp2p.io/tcp/443/wss/ipfs/Qmbut9Ywz9YEDrz8ySBSgWyJk41Uvm2QJPhwDJzJyGFsD6',
//     '/dns4/node0.preload.ipfs.io/tcp/443/wss/ipfs/QmZMxNdpMkewiVZLMRxaNxUeZpDUb34pWjZ1kZvsd16Zic',
//     '/dns4/node1.preload.ipfs.io/tcp/443/wss/ipfs/Qmbut9Ywz9YEDrz8ySBSgWyJk41Uvm2QJPhwDJzJyGFsD6'
]

let node ;

function nPeers( ) {
        return node.metrics.peers.length
}


function displayNodePeerInfo ()  {
    let peerInfo = node.peerInfo
    debug ("--- node's peer info")
    peerInfo.multiaddrs.forEach( m => {
        debug (m.toString() ) 
    });
    debug ("--- ")          
}

async function initNode(id, ishop:boolean = false) {
    let pi = new PeerInfo(id); 
    let port = ishop? _PORT : 0

    pi.multiaddrs.add(`/ip4/0.0.0.0/tcp/${port}`);
    pi.multiaddrs.add( multiaddr(`/p2p/${id.toB58String()}`))
    // pi.multiaddrs.add(`/ip4/0.0.0.0/tcp/${port}/p2p/${id.toB58String()}`);

    if (!ishop) {      
      bootstrapers.unshift (`/dns4/troupe-lbs-primary.askarov.net/tcp/${_PORT}/p2p/`)       
    }

    node = await Libp2p.create ({     
      peerInfo : pi,  
      modules: {
        transport: [TCP],
        streamMuxer: [Multiplex],
        connEncryption: [SECIO],
        peerDiscovery: [MulticastDNS,Bootstrap],
        dht: KadDHT
      },
      metrics: {enabled: true},
      dialer: {
        maxParallelDials: 150, // How many multiaddrs we can dial in parallel
        maxDialsPerPeer: 8, // How many multiaddrs we can dial per peer, in parallel
        dialTimeout: 10e3 // 15 second dial timeout per peer
      },
      config: {
        peerDiscovery: {
          autoDial:true,
          mdns: {
            interval: 1e3,
            enabled: true
          },
          bootstrap: {
            interval: 1000,
            list : bootstrapers,
            enabled: true
          }
        },        
        dht: {
          enabled: false,
          kBucketSize: 20,             
        },

        relay:{enabled: true, hop :  { enabled: ishop, active:ishop } } 
      }
    })

    node.on('error', (err) => {
            error (`Error in p2p: ${err}`);        
        })        
        

    node.on('peer:discovery', (peer) => {
      // debug (`Discovered: ${peer.id.toB58String()}`)
    })
        
    node.on ('peer:connect', (peerInfo)=>{
        tryPending ();
        let idStr = peerInfo.id.toB58String();            
        debug (`++ Connect: ${idStr}   ${nPeers()}` );
    })

    

    await node.start();

    debug ("Node created");

    displayNodePeerInfo()
    return node;
}

let __networkPending = [];

function addPending (t) {
    __networkPending.push (t);
}

function tryPending() {
      if (__networkPending.length > 0 ) {            
          let n = __networkPending.length;

          debug (`# # # # Connect trigger: try Pending ${n}`);

          for ( let i = 0; i < n; i++) {
              // debug (`discovery trigger ${key}`)
              let t = __networkPending.shift ();
              t () ;
          }
      }
}

async function getPeerInfo(id) {
      const peerId = id
      return new Promise ((resolve, reject) => {
          let n_attempts = 0;
          async function try_find_peer ()  {                                       
            if (node.peerStore.has(peerId)) {
                debug ("peer info is in the store")
                resolve (node.peerStore.get(peerId))                                     
            } else {
              try  {                    
                  debug ("calling peerRouting.findPeer")
                  const peerInfo = await node.peerRouting.findPeer (peerId, {timeout:1000}); 
                  debug ("findPeer returned")
                  resolve (peerInfo);                  
              } catch (err) {                           
                  debug (`try_find_peer exception`) 
                  if (nPeers() > 0 ) {
                      n_attempts ++ ;
                  }

                  if (err instanceof AggregateError) {
                      for (let e of err) {
                          debug (`Find peer error with code: ${e}, ${e.code}`)
                      }
                  } else {
                      debug (`Find peer error: ${err.toString()}`)
                      throw err;
                  }

                  if (n_attempts > 5) {
                      resolve (new PeerInfo(id))
                      // reject (err);h
                  } else {
                      debug (`try_find_peer: attempt ${n_attempts} failed with ${nPeers()} nodes connected`)
                      addPending (try_find_peer);
                  }   
              }
            }
          }
          try_find_peer ();
      });        
}


async function getPeerInfoWithRelay(id) {  
    let pi:any = await getPeerInfo (id)
    console.log ("RELAY ID IS" , _relay_id)
    if (_relay_id) {
      pi.multiaddrs.add( multiaddr(`/p2p/${_relay_id}/p2p-circuit/p2p/${id.toB58String()}`))
    }
    return pi
}

async function dialNode (id) {
      function tryDial () {
        let i = 0;
        let timeout = 2000
        return new Promise((resolve, reject) =>{
            async function iterate() {
                try {
                    let peerInfo : any = await getPeerInfoWithRelay (id);    
                    debug ("PEER INFO OBTAINED")
                    debug (`>> Dialing to:`)
                        peerInfo.multiaddrs.forEach( m => {
                            debug (m.toString() ) 
                        });
                        debug (">> << ")        
                    
                    debug (`trying to dial, attempt number {i}`)
                    const { stream } = await node.dialProtocol(peerInfo, _PROTOCOL)
                    resolve ( stream );
                } catch ( err ) {
                    processExpectedNetworkErrors (err);
                    // if the error is suppressed we move on to trying 10 times
                    // with exponential backoff
                    // 2020-02-10; AA: TODO: this code has a hardcoded constant 
                    if (i <= 10) {
                        debug (`dial failed, going to retry in ${timeout} seconds`)
                        setTimeout (iterate, timeout )
                        timeout *= 2
                        i ++ ;
                    } else {
                        reject (err);
                    } 
                }
            }
            iterate ()                
        })
      }
      let stream = await tryDial ()
      debug (">> dial successful << ")

      debug ("DIAL OK");
}

function processExpectedNetworkErrors (err) {    
  if (err instanceof AggregateError) {
    for (const e of err ) {        
      processExpectedNetworkErrors (e)
    }
  } else {
    switch (err.code) {
      case 'ENETUNREACH':
        error (`${err.toString()}`)
        break;
      case 'ENOTFOUND':
        error (`${err.toString()}`)
        break;
      case 'ECONNRESET':
        error (`${err.toString()}`)
        break;
      case 'ERR_TRANSPORT_DIAL_FAILED':
        error (`${err.toString()}`)
        break;
      case 'ABORT_ERR':
        error (`${err.toString()}`)
        break;
      case 'ECONNREFUSED':
        error ((`${err.toString()}`))
        break;
      case 'ERR_HOP_REQUEST_FAILED':
        error ((`${err.toString()}`))
        break;
      case 'ERR_NO_DIAL_MULTIADDRS':
        break;
      default:
        error (`Unhandled error case ${err.code}`)
        throw err;
    }    
  }    
}




async function getIdFromFile (fname) {
  let s = await fs.promises.readFile(fname, {encoding:'utf8'});
  let json = await JSON.parse (s);
  let id = await PeerId.createFromJSON(json);
  return id;
}

let _keepAliveCounter = 0;
let _relay_id = null;
async function keepAliveRelay (id) {
  async function f ()  {
    try {
      const conn = await node.dial(id);
      const {stream} = await node.dialProtocol (id, "/trouperelay/keepalive")
      debug (`the stream is ${stream}`)
      const peerId = conn.remotePeer
      _relay_id = peerId.toB58String()
      debug (`dialed to relay ${_keepAliveCounter++}`)  
      pipe ( stream
           , async (source) => { 
                for await (const msg of source ) {
                  console.log (`relay says ${msg}`)
                }
            }
          );
    } catch (err ) {
      if (err instanceof AggregateError) {
        for (let e of err) {
            debug (`Error dialing relay server: ${e.code}`)
        }
      }
      else {
         error (`error dialing relay ${err}`);
      }
    }
    setTimeout(f, 5000)
  }
  f ()    
}


async function main () {    
    let serverid = await getIdFromFile (yargs.argv.serverid)     
    let listenerid ;
    switch (yargs.argv.mode) {
        case 'relay': 
            await initNode (serverid, true)            
            await node.handle("/trouperelay/keepalive", async ({connection, stream}) => {
              debug (`relay alive signal on ${connection.toString()}`);
              pipe (['hello from Nodejs relay'], stream.sink)
            })
            break;
        case 'listener':
            debug ("listener")
            listenerid = await getIdFromFile (yargs.argv.listenerid)
            await initNode(listenerid, false) 
            // keepAliveRelay ("QmWJeEW5cdCtx7BNTMiJR7forsWP3VcDFup5aRpFFPY123");
            keepAliveRelay(`/dns4/troupe-lbs-primary.askarov.net/tcp/${_PORT}/`)
            await node.handle(_PROTOCOL, async ({connection, stream, protocol}) => {
                debug ("PROTOCOL CONNECTION")
            }) 

            break;
        case 'dialer':
            debug ("listener")
            let dialerid = await PeerId.create();
            listenerid = await getIdFromFile (yargs.argv.listenerid) 
            await initNode(dialerid, false)
            // keepAliveRelay ("QmWJeEW5cdCtx7BNTMiJR7forsWP3VcDFup5aRpFFPY123");
            keepAliveRelay(`/dns4/troupe-lbs-primary.askarov.net/tcp/${_PORT}/`)
            await dialNode(listenerid)
    }
}


process.on('unhandledRejection', processExpectedNetworkErrors)
process.on('uncaughtException', processExpectedNetworkErrors); 


main ()