/*

The p2p runtime maintains a lookup table that maps each PeerId that we see to a
Publishable object. We populate this table upon the following events:

1. When we initate a connection to a node upon the first time, via
   node.dialProtocol because we are about to send a message (spawn or send) to
   that PeerId.

2. When we receive a connection from some other in the callback passed to the
   node.handle. In this case it is just some other node that wants to connect to
   us and we are going to "cache" the connection.

Each of these Publishable objects is piped to the connection that is obtained in
the respective cases above.

Additionally, we pipe the connection to a drain sink that does the following:

1. Receive messages from the remote node: this is the normal opeeration. Upon
   receiving a message it will then communicate it back to our runtime (probably
   to the serialization layer) similarly to the way we handle messages now.

2. Upon the end of the pipe message we will also set up a handler that removes
   the publishable object from the lookup table. This should ensure that if we
   we want to communicate with that node later, we will dialing to that node.

The messages themselves can be one of the three forms: 

1. SPAWN -- spawning on a remote node

2. SPAWNOK -- reply to the spawn (we did not have an analogue of this in express
   runtime because express was giving up the possibility of sending response to
   a given request, here we are doing it manually by maintaining a spawnNonce
   map).

3. SEND -- sending a message

4. TEST -- for testing/development purposes

Note that libp2p already implements stream multiplexing and we could
have avoided maintaining these tables and use the built-in
multiplexing instead but 

  (a) it appears to be slower than our custom-build mapping, and 

  (b) keeping track of each connection should allow to implement
  process linking.

Note on the code below: the code below uses the libp2p framework, and is
partially grown out of the Chat example in that framework (to make sense of the
control flow transfer in this part of the runtime it may be helpful look up of
the libp2p).

*/




'use strict'

// LOGGING AND DEBUGGING 

let yargs = require('yargs');
let logLevel = yargs.argv.debugp2p?'debug':'info';
const _PROTOCOL = "/troupe/1.0.0"


const logger = require('../logger.js').mkLogger('p2p',logLevel);
const info = x => logger.info(x)
const debug = x => logger.debug(x)
const error = x => logger.error(x);

const lp = require('it-length-prefixed')

debug ("this should show up")


// 
// const pull = require('pull-stream')
const pipe = require('it-pipe')
const pushable = require ('./Pushable.js')
import { map } from 'streaming-iterables'

const PeerId = require("peer-id")
const PeerInfo = require("peer-info")
const Libp2p = require("./libp2p-bundle")
const multiaddr = require('multiaddr')
const AggregateError = require('aggregate-error');
const p2pconfig = require('./p2pconfig.js')




// const Pushable = require('pull-pushable')

import { v4 as uuidv4} from 'uuid'


const MessageType = {
    SPAWN: 0,
    SPAWNOK: 1,
    SEND: 2,
    TEST: 3,
    WHEREIS: 4,
    WHEREISOK: 5    
}



async function obtainPeerId(nodeId) {
    let id = null;
    if (nodeId) {
        try {
            id = await PeerId.createFromJSON(nodeId);
            debug(`Loaded id from file: ${id.toB58String()}`);
        } catch (err ) {
            logger.error("error creating peer id from json");
            throw err        
        }
    } else {
        try {
            debug ("Creating new pair id...")
            id = await PeerId.create();
            debug("Created new pair id");
        } catch (err) {
            logger.error("error creating new peer id");
            throw err;
        }
    }
    let _peerInfo = new PeerInfo(id)    
    _peerInfo.multiaddrs.add(`/ip4/0.0.0.0/tcp/0`);     
    return _peerInfo;
}


export interface IHash {
    [details: string] : any;
} 



function TroupeP2P (_rt, _peerInfo) {
    // GLOBALS 
    

    let _node = null; // the current node; initalized once upon start
    
    let _nodeTable:IHash = {}; // a table of the form [PeerId ↦ Connection]
    // This table is mutable: we populate it upon
    // discover of a node (either via dial or
    // handle), and remove entries upon
    // disconnects.


    let _spawnNonces = {}; // a table of the form [Nonce ↦ Stream].
    // This table is mutable: we populate it upon
    // issing a SPAWN to a remote node, and remove
    // entries from it upon receiving a successful
    // SPAWNOK mesasge that matches the nonce.

    let _whereisNonces  = {}; 

    let _unacknowledged:any = {}

    function addUnacknowledged (id, uuid, f) {
        if (!_unacknowledged[id]){ 
            _unacknowledged[id] = []
        }
        _unacknowledged[id][uuid] = f
    }

    function removeUnacknowledged (id, uuid) {
        delete _unacknowledged[id][uuid];
    }


    function reissueUnacknowledged (id:string) {
        for (let uuid in _unacknowledged[id] ) {
            setImmediate(_unacknowledged[id][uuid])
        }
    }

    function setupConnection (peerId, stream) {        
        let id:string = peerId.toB58String()
        debug(`setupConnection with ${id}`);
        _node.connectionManager.setPeerValue (peerId, 1);
        const p = pushable () 
        _nodeTable[id] = p;              

        pipe (p, map (JSON.stringify), lp.encode(), stream,lp.decode(), map(JSON.parse), 
            async (source) => {
                try {
                    for await (const x of source) {
                        inputHandler (id, x, peerId)
                    }
                } catch (err) {
                    debug (`try catch of the source`)
                    processExpectedNetworkErrors(err, "setupConnection/pipe");
                }
                
                debug(`deleting entry for  ${id}`);                
                try {
                  await _node.hangUp (peerId); // hanging up; will it cause an exception?? 
                } catch (err) {

                }
                delete _nodeTable[id];        
                reissueUnacknowledged (id)  
            }
        )            
    }
   
    
    

    async function push_wrap (id:any, data:any) {
        
        while (true) {
            try {
                debug (`push_wrap`)

                if (!_nodeTable[id]) {     
                    debug (`no stream cached for ${id}; redialing}`)    
                    await dial (id);            
                } else {
                    debug (`cached stream is available; we reuse it`)
                }
                
                let p = _nodeTable[id];

                debug (`push_wrap; stream obtained`)
                await p.push (data);
                debug (`push_wrap; data pushed into the stream`)
                break 
            } catch (err) {
                // the stream we have used is 
                // no good for whatever reason; 
                // most likely there are networking 
                // issues. we report the errors
                // and redial
                debug (`push wrap error`)
                processExpectedNetworkErrors(err, "push_wrap");                
            }            
        }
    }
  


    this.spawnp2p = async (id, data) => {
        const spawnNonce = uuidv4();
        return new Promise ((resolve, reject) => {
            _spawnNonces[spawnNonce] = (err, data) => {
                if (err) { reject (err) } else { resolve (data)}
            };
            push_wrap(id,  {
                messageType: MessageType.SPAWN,
                spawnNonce: spawnNonce,
                message: data
            })
        });
    }

    
    this.whereisp2p = async (id, str) => {            

        let whereisNonce = uuidv4()

        function f () {
            push_wrap(id, {
                messageType: MessageType.WHEREIS,
                whereisNonce : whereisNonce,
                message : str 
            });        
        }
        addUnacknowledged(id, whereisNonce,f);
        
        return new Promise ( (resolve, reject) => {        
            _whereisNonces[whereisNonce] = (err, data) => {
                if (err) { reject (err) } else { 
                    removeUnacknowledged (id,whereisNonce)
                    resolve (data)
                } 
            }
            debug ("pushing whereis message")
            f ();    
        })
    }

    this.sendp2p = async (id, procId, obj) => {
        // let p = await getNodePushStream(id);
        push_wrap(id, {
            messageType: MessageType.SEND,
            pid: procId,
            message: obj
        });
    }

    
    let __networkPending = [];

    function tryPending() {
        if (__networkPending.length > 0 ) {            
            let n = __networkPending.length;
            debug (`####### Connect trigger: try Pending. There are ${n} pending commands`);
            for ( let i = 0; i < n; i++) {
                // debug (`discovery trigger ${key}`)
                let t = __networkPending.shift ();
                t () ;
            }
        }
    }

    function addPending (t) {
        __networkPending.push (t);
    }

    

    async function getPeerInfo(id:string) {
        const peerId = PeerId.createFromB58String(id); 
        return new Promise ((resolve, reject) => {
            let n_attempts = 0;
            async function try_find_peer ()  {                                       
              if (_node.peerStore.has(peerId)) {
                  debug ("peer info is in the store")
                  resolve (_node.peerStore.get(peerId))                                     
              } else {
                try  {                    
                    debug (`calling peerRouting.findPeer ${peerId}`)
                    const peerInfo = await _node.peerRouting.findPeer (peerId, {timeout:1000}); 
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
                        debug (`Resolving to empty peer info`)
                        resolve (new PeerInfo(peerId))
                        // reject (err);
                    } else {
                        debug (`try_find_peer: attempt ${n_attempts} failed with ${nPeers()} nodes connected`)
                        // addPending (try_find_peer);
                        setTimeout (try_find_peer, 500)
                    }   
                }
              }
            }
            try_find_peer ();
        });        
    }
    
    let _relay_id = null;

    async function getPeerInfoWithRelay(id:any) {  
        let pi:any = await getPeerInfo (id)
        if (_relay_id) {
            pi.multiaddrs.add( multiaddr(`/p2p/${_relay_id}/p2p-circuit/p2p/${id}`))
          }
        
        // for (let i = 0; i < p2pconfig.relays.length; i++  ) {
        //     pi.multiaddrs.add( multiaddr(`${p2pconfig.relays[i]}/p2p-circuit/p2p/${id}`))
        // }
        return pi  
    }

    function dial (id)  {                 
        let i = 0;      
        let timeout = 2000;      
        return new Promise((resolve, reject) =>{
            async function iterate() {
                try {
                    const peerInfo = await getPeerInfoWithRelay(id);
                    debug ("find peer succeeded");        
                    debug (`dialing will use the following addresses:`)
                    peerInfo.multiaddrs.forEach( m => {debug (m.toString() ) });
                    debug (">> -- end of address list -- << ")
                    debug (`trying to dial, attempt number ${i}`)
                    const { stream } = await _node.dialProtocol(peerInfo, _PROTOCOL)
                    debug ("dial successful")        
                    setupConnection (peerInfo.id, stream);
                    resolve ( stream );
                } catch ( err ) {
                    processExpectedNetworkErrors (err, "dial");
                    // if the error is suppressed we move on to trying 10 times
                    // with exponential backoff
                    // 2020-02-10; AA: TODO: this code has a hardcoded constant 
                    if (i <= 10) {
                        debug (`dial failed, we retry in ${timeout} seconds`)                         
                        setTimeout (iterate, timeout);
                        i ++ ;
                        timeout *= 2
                    } else {
                        debug (`we are giving up on dialing`)
                        reject (err);
                    } 
                }
            }
            iterate ()                
        })
    }        
    
    
    



    /*
    async function getNodePushStream (id:string) {            
        if (!_nodeTable[id]) {     
            debug (`no stream cached for ${id}; redialing}`)    
            await dial (id);            
        } 
        
        return _nodeTable[id];
    }
    */


    async function inputHandler(id, input, fromNodeId_) {
        let fromNodeId = fromNodeId_.toB58String()
        debug ("-- input handler")
        switch (input.messageType) {
            case (MessageType.SPAWN):
            if (_rt.remoteSpawnOK()) {
                debug ("RECEIVED SPAWN")
                let x = await _rt.spawnFromRemote (input.message, fromNodeId)

                push_wrap(id, {
                    messageType: MessageType.SPAWNOK,
                    spawnNonce: input.spawnNonce,
                    message: x
                });
                break;
            } // drop the message otherwise

            case (MessageType.SPAWNOK):
                debug ("SPAWN OK")
                let _cb = _spawnNonces[input.spawnNonce];
                if (_cb) {
                    delete _spawnNonces[input.spawnNonce]; // cleanup
                    _cb(null, input.message); // null means no errors            
                } else {
                    // something is fishy;
                    debug("something is fishy; no matching callback for the nonce");
                }
                break;

            case (MessageType.SEND):
                debug (`SEND  ${fromNodeId_.toB58String()}`);
                _rt.receiveFromRemote (
                    input.pid,
                    input.message,
                    fromNodeId
                )
                break;

            case (MessageType.WHEREIS): 
                debug ("p2p whereis incoming request")
                let y = await _rt.whereisFromRemote (input.message)
                push_wrap(id, {
                    messageType: MessageType.WHEREISOK,
                    whereisNonce : input.whereisNonce, 
                    message : y
                });
                debug ("p2p whereis replied")
                break; 
                
            case (MessageType.WHEREISOK): 
                let _cbw = _whereisNonces[input.whereisNonce];
                if (_cbw) {
                    delete _whereisNonces [input.whereisNonce];
                    _cbw (null, input.message);
                } else {
                    debug ("cannot find whereis callback")
                }
                break;

            case (MessageType.TEST):
                debug("TEST input");
                break;
                
            default:
                debug (`received data ${input.toString('utf8').replace('\n', '')}`);
                break;
        }
    }



    let _KEEPALIVE = 5000
    let _keepAliveCounter = 0;
    let _relayTable:IHash = {}

    async function dialRelay (relay_addr)  {
      const conn = await _node.dial(relay_addr);
      const {stream} = await _node.dialProtocol (relay_addr, "/trouperelay/keepalive")            
      const peerId = conn.remotePeer
      _relay_id = peerId.toB58String()
      //   debug (`~~ relay dialed, keep alive counter is ${_keepAliveCounter++}`)  
      const p = pushable()
      
      pipe (stream.source, 
              async (source: any) => { 
                 let ss = "" 
                 for await (const msg of source ) {                   
                  // debug (`~~ relay says:${msg.toString().trim()}`)  
                 }                
           })
      pipe (p, stream.sink);
      let id = relay_addr.split('/').pop();
      _relayTable[id] = p ;
      return p;
    }
    
    async function keepAliveRelay (relay_addr:string) {
      let id = relay_addr.split('/').pop();
      // debug (`id is ${id}`)
      let timeout = _KEEPALIVE;
      async function f ()  {
        try {            
            let p = _relayTable[id] ? _relayTable[id] : await dialRelay (relay_addr)            
            p.push (`keep alive request ${_keepAliveCounter++}\n`)
            timeout = _KEEPALIVE
          } catch (err ) {
            timeout = timeout < 600e3 ? timeout * 2 : timeout // exponential backoff with 10 min limit            
            processExpectedNetworkErrors(err, "relay")
            debug (`~~ error reaching the relay server; we will retry again in ${timeout/1000} seconds`)
          }             
          setTimeout(f,timeout) 
        }
        f ()    
      }
      
    


    function onPeerDiscovery (peer)  {       
        debug(`discovered: ${peer.id.toB58String()}`)        
    }

    function nPeers( ) {
        return _node.metrics.peers.length
    }
 
    this.startNode = async () => {    
        debug(`Starting p2p node ${_peerInfo.id.toB58String()}`)
        _node = new Libp2p ( 
          {   peerInfo: _peerInfo}
        );
        debug("Node created")
        
        _node.on('peer:discovery', onPeerDiscovery );
    
        _node.on('error', (err) => {
            error (`Error in p2p: ${err}`);        
        })        
        
        _node.on ('peer:connect', (peerInfo)=>{
            tryPending ();
            let idStr = peerInfo.id.toB58String();            
            debug (`++ connect: ${idStr}   ${nPeers()}` );
        })

        _node.on('peer:disconnect', (peerInfo) => {
            let id = peerInfo.id.toB58String();
            debug (`-- disconnect: ${id}`)
            if (_nodeTable[id]) {
                debug (`deleting node table entry for ${id}`)
              delete _nodeTable[id]
            }
            if (_relayTable[id]) {
              debug (`deleting relay table entry`)
              delete _relayTable[id]
            }            
        }) 
       
        await _node.handle(_PROTOCOL, async ({ connection, stream }) => {                
                setupConnection (connection.remotePeer, stream)
            }        
        )
        
        await _node.start ();
        debug("p2p node started")
        for (let relay_addr of p2pconfig.relays ) {
            keepAliveRelay(relay_addr);
        }
          
        /*
        function showNPeers ()  {
            if (logLevel == 'debug') {
                debug(`--------- n peers: ${nPeers()}`);
                setTimeout(showNPeers, 5000);
            }
        }
        showNPeers();
        */
        return;
    }


    this.stop = async () => {        
        debug ("node stopping...")
        await _node.stop();
        debug ("node stopped")        
    }     
}


const _HEALTHCHECKPERIOD = 5000  // 2020-02-10; AA; this should be an option 

function setupBlockingHealthChecker (period) {    
    let _lastHealth:number = Date.now()
    let _healthCounter = 0;
    let health_threshold = Math.max (period * 1.25 , period + 50)  
    // AA: 2020-02-10;
    // The event queue always has a fair bit of latency, so we adjust for 
    // the minimal expected latency here; the constant of 50 is an
    // empirically derived value, but needs to be critically reevaluated
    // as the system evolves

    function f()  {
        let now = Date.now()
        // debug (`Health checker running ${now - _lastHealth}, ${new Date()}`)
        if (now - _lastHealth > health_threshold) {
          debug (`Potential blocking issue: ${_healthCounter} ${now - _lastHealth}`)
        }
        _lastHealth = now;
        setTimeout(f, period);
      }    
    f ()  
}


let _troupeP2P = null;

async function startp2p(nodeId, rt) {            
    // kick off the network initialization by loading the peerid
    let peerInfo = await obtainPeerId(nodeId);
    debug ("Peer info created/loaded")
    _troupeP2P = new TroupeP2P( rt, peerInfo );        
    setupBlockingHealthChecker (_HEALTHCHECKPERIOD)
    await _troupeP2P.startNode ();
    if (rt) {
      rt.networkReady(peerInfo.id.toB58String());
    }
}


function processExpectedNetworkErrors (err, source="source unknown") {    
    debug (`error source: ${source}`);
    if (err instanceof AggregateError) {
      for (const e of err ) {        
        processExpectedNetworkErrors (e, source)
      }
    } else {
      if (err.code) {
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
            error ((`${err.toString()}`))
            break;
          case 'ERR_ENCRYPTION_FAILED':
            error ((`${err.toString()}`))
            break;
          case 'ERR_NO_VALID_ADDRESSES':
            error ((`${err.toString()}`))
            break;  
          case 'ERR_MPLEX_STREAM_RESET':
            error ((`${err.toString()}`))
            break;  

          default:
            error (`Unhandled error case with error code ${err.code}`)
            throw err;
        }    
      } else {
           error (`Unhandled general error case ${err}`)
           throw err;
      }
    }    
  }


module.exports = {
    startp2p: startp2p,
    spawnp2p: (arg1, arg2) => _troupeP2P.spawnp2p(arg1, arg2),
    sendp2p: (arg1, arg2, arg3) => _troupeP2P.sendp2p(arg1, arg2, arg3),
    whereisp2p: (arg1, arg2) => _troupeP2P.whereisp2p (arg1, arg2),
    stopp2p: async () =>{
            await _troupeP2P.stop()
    },
    processExpectedNetworkErrors: processExpectedNetworkErrors
}
