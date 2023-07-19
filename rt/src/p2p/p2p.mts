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

// IMPORTS

import { PeerId } from '@libp2p/interface-peer-id';
import yargs from 'yargs';
import { tcp } from '@libp2p/tcp';
import { webSockets } from '@libp2p/websockets';
import { mplex } from '@libp2p/mplex';
import { yamux } from '@chainsafe/libp2p-yamux';
import { noise } from '@chainsafe/libp2p-noise';
import defaultsDeep from '@nodeutils/defaults-deep';
import { Libp2p, createLibp2p as create } from 'libp2p';
import { createFromJSON, createEd25519PeerId } from '@libp2p/peer-id-factory';
import { peerIdFromString } from '@libp2p/peer-id';
import { bootstrap } from '@libp2p/bootstrap';
import { mdns } from '@libp2p/mdns';
import { pipe } from 'it-pipe';
import * as lp from 'it-length-prefixed';
import map from 'it-map';
import { fromString as uint8ArrayFromString } from 'uint8arrays/from-string';
import { toString as uint8ArrayToString } from 'uint8arrays/to-string';
import { pushable } from 'it-pushable';
import p2pconfig from './p2pconfig.mjs';
import { multiaddr } from '@multiformats/multiaddr';
import { identifyService } from 'libp2p/identify';
import { circuitRelayTransport } from 'libp2p/circuit-relay';
import { KEEP_ALIVE } from '@libp2p/interface-peer-store/tags';
import { Logger } from 'winston';
import {v4 as uuidv4} from 'uuid';

// LOGGING AND DEBUGGING 

let logLevel = yargs.argv.debugp2p? 'debug':'info';
let __port = yargs.argv.port || 0;

let logger: Logger;
(async() => {
  let { mkLogger } = await import ('../logger.mjs');
  logger = mkLogger ('p2p', logLevel);
})()

const info = x => logger.info(x);
const debug = x => logger.debug(x);
const error = x => logger.error(x);

// CONSTANTS

const _PROTOCOL = "/troupe/1.0.0";
const _RELAY_PROTOCOL = "/trouperelay/keepalive";
const _HEALTHCHECKPERIOD = 5000;  // 2020-02-10; AA; this should be an option
const _KEEPALIVE = 5000;
const MessageType = {
    SPAWN: 0,
    SPAWNOK: 1,
    SEND: 2,
    TEST: 3,
    WHEREIS: 4,
    WHEREISOK: 5    
};

// SET-UP

let _node : Libp2p = null;
let _rt = null;

let bootstrappers = [ 
  '/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmbLHAnMoJPWSCR5Zhtx6BHJX9KiKNN6tpvbUcqanj75Nb',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmZa1sAxajnQjVM8WjWXoMbmPd7NsWhfKsPkErzpm9wGkp',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmQCU2EcMqAqQPR2i9bChDtGNJchTbq5TbXJJ16u19uLTa',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmcZf59bWwK5XFi76CZX8cbJ4BhTzzA3gU1ZjYZcYW3dwt',
  '/ip4/134.122.54.216/tcp/5555/p2p/QmcQpBNGULxRC3QmvxVGXSw8BarpMvdADYvFtmvKAL5QMe',
];
let bootstrappers2 = p2pconfig.known_nodes.map((obj) => `${obj.ip}/${obj.nodeid}`); //AB: bootstrap known_nodes from config?? Make findNode easier

async function startp2p(nodeId, rt: any): Promise<PeerId> {            
  let id : PeerId = await obtainPeerId(nodeId);

  try {
    let nodeListener: Libp2p = await createLibp2p({
      peerId: id,
      addresses: {
        listen: [`/ip4/0.0.0.0/tcp/${__port}`]
      },
      connectionManager : {
        maxConnections: Infinity,
        minConnections: 0 //What are good numbers here?
      }
    });
  
    _node = nodeListener;
    _rt = rt;

  } catch (err) {
    error(`Something wrong while creating Libp2p node: ${err}`);
    throw err;
  }
  
  await _node.handle(_PROTOCOL, async ({ connection, stream }) => {
    debug(`Handling protocol dial from id: ${connection.remotePeer}`);
    setupConnection(connection.remotePeer, stream);
  });

  _node.addEventListener('peer:discovery', async (evt) => {
    const peerInfo = evt.detail;
    debug(`Discovered: ${peerInfo.id.toString()}`);
  });

  _node.addEventListener('peer:connect', (evt) => {
    const peerId = evt.detail;
    debug(`Connection established to: ${peerId.toString()}`);
  });

  _node.addEventListener('peer:disconnect', (evt) => {
    let id = evt.detail;
    debug (`-- disconnect: ${id}`);
    if (_relayTable[id.toString()]) {
      debug (`deleting relay table entry`);
      delete _relayTable[id.toString()];
    }            
  });
  
  _node.addEventListener('self:peer:update', (_) => {
    debug(`Advertising with following addresses:`);
    _node.getMultiaddrs().forEach(m => debug(m.toString()));
  })

  debug("p2p node started");
  debug(`id is ${id.toString()}`);

  setupBlockingHealthChecker(_HEALTHCHECKPERIOD);

  /*for (let relay_addr of p2pconfig.relays ) { //AB: update relay addresses in p2pconfig
    keepAliveRelay(relay_addr);
  }*/ //AB: update relay addresses in p2pconfig 
  keepAliveRelay("/ip4/134.209.92.133/tcp/5555/ws/p2p/12D3KooWShh9qmeS1UEgwWpjAsrjsigu8UGh8DRKyx1UG6HeHzjf");

  return id;
}

async function createLibp2p(_options) {
  const defaults = {
    transports: [
      tcp(),
      webSockets(),
      circuitRelayTransport({
        discoverRelays: 2, //AB: what to set this to?
        reservationConcurrency : 2,
      })
    ],
    streamMuxers: [
      yamux(),
      mplex()
    ],
    connectionEncryption: [
      noise(),
    ],
    peerDiscovery: [
      /*bootstrap({
        list: bootstrappers
      }),*/ //AB: should we use bootstrappers?
      /*mdns({
        interval: 20e3
      })*/
    ],
    services: {
      identify: identifyService()
    },
    /*dht: kadDHT({
      kBucketSize: 20,
      clientMode: true // Whether to run the WAN DHT in client or server mode (default: client mode)
    }),*/ //AB: use this or not?
  };

  return create(defaultsDeep(_options, defaults));
}

async function obtainPeerId(nodeId): Promise<PeerId> {    
  let id : PeerId = null;
  if(nodeId) {
    try {
      id = await createFromJSON(nodeId);
      debug(`Loaded id from file: ${id.toString()}`);
    } catch (err) {
      error("Error creating peer id from json");
      throw err;    
    }
  } else {
    try {
      debug("Creating new peer id...");
      id = await createEd25519PeerId();
      debug("Created new peer id");
    } catch (err) {
      error("Error creating new peer id");
      throw err;
    }
  }

  return id;
}

// DIAL

function dial(id) {                 
  let i = 0;      
  let timeout = 2000;      
  return new Promise((resolve, reject) => {
    async function iterate() {
      try {
        let peerId : PeerId = await getPeerInfoWithRelay(id);

        debug (`trying to dial ${peerId}, attempt number ${i}`);
        const stream = await _node.dialProtocol(peerId, _PROTOCOL);
        debug ("dial successful");

        setupConnection (peerId, stream);

        resolve(stream);
      } catch (err) {
        processExpectedNetworkErrors (err, "dial");

        // if the error is suppressed we move on to trying 10 times with exponential backoff
        // 2020-02-10; AA: TODO: this code has a hardcoded constant 
        if(i <= 10) {
          debug (`dial failed, we retry in ${timeout} seconds`);
          error(err);
          setTimeout(iterate, timeout);
          i++;
          timeout *= 2;
        } else {
          debug (`we are giving up on dialing`);
          error(err);
          reject(err);
        } 
      }
    }
    iterate();                
  });
}

async function getPeerInfoWithRelay(id:any) {
  let known_nodes = p2pconfig.known_nodes;
  for(let ni of known_nodes) {
    if(ni.nodeid == id) {
      // found a known node!
      let pi = peerIdFromString(id);
      await _node.peerStore.patch(pi, {
        multiaddrs: [
          multiaddr(`${ni.ip}`)
        ]
      });
      debug(`node ${ni.nodeid} will be contacted directly via IP: ${ni.ip}`);
      return pi;
    }
  }

  debug("the node is not known; using relay information");
  let pi = await getPeerInfo(id);

  if(_relay_id) { //AB: use all relays as below!
    await _node.peerStore.patch(pi, {
      multiaddrs: [
        multiaddr(`/p2p/${_relay_id}/p2p-circuit/p2p/${id}`)
      ]
    });
  }
  
  // for (let i = 0; i < p2pconfig.relays.length; i++  ) {
  //     pi.multiaddrs.add( multiaddr(`${p2pconfig.relays[i]}/p2p-circuit/p2p/${id}`))
  // }
  return pi;
}

async function getPeerInfo(id:string) : Promise<PeerId>{
  const peerId = peerIdFromString(id);

  return new Promise ((resolve, reject) => {
    let n_attempts = 0;
    async function try_find_peer() {
      if(await _node.peerStore.has(peerId)) {
        debug("peer info is in the store");
        try {
          let foundPeer = await _node.peerStore.get(peerId);
          resolve(foundPeer.id);
        } catch (err) {
          error(`Error in getPeerInfo / peerStore.get: ${err}`);
          throw err;
        }
      } else {
        try {                    
          debug(`calling peerRouting.findPeer ${peerId}`);
          const peerInfo = await _node.peerRouting.findPeer(peerId, {signal : AbortSignal.timeout(1000)}); 
          debug ("findPeer returned");
          await _node.peerStore.patch(peerInfo.id, {
            multiaddrs: 
              peerInfo.multiaddrs
          }); //AB: necessary??
          debug("added multiaddr to store");
          resolve (peerInfo.id);
        } catch (err) {                           
          debug(`try_find_peer exception`);

          if(nPeers() > 0) { //AB: why only when nPeers > 0?
            n_attempts++;
          }

          if(err instanceof AggregateError) {
            for(let e of err.errors) {
              debug(`Find peer error with code: ${e}, ${e.code}`);
            }
          } else {
            debug(`Find peer error: ${err.toString()}`);
          }

          if(n_attempts > 5) {
            debug(`Resolving to empty peer info`);
            resolve(peerId);
          } else {
            debug(`try_find_peer: attempt ${n_attempts} failed with ${nPeers()} nodes connected`);
            setTimeout(try_find_peer, 500);
          }
        }
      }
    }
    try_find_peer();
  });        
}

function nPeers() {
  return _node.getPeers().length;
}

// CONNECTION SET-UP

function setupConnection(peerId : PeerId, stream): void {        
  let id: string = peerId.toString();
  debug(`setupConnection with ${id}`);
  const p = pushable({ objectMode : true });

  pipe (p,
        (source) => map(source, (json) => JSON.stringify(json)),
        (source) => map(source, (string : string) => uint8ArrayFromString(string, 'utf8')),
        (source) => lp.encode(source),
        stream,
        (source) => lp.decode(source),
        (source) => map(source, (buf) => uint8ArrayToString(buf.subarray())),
        (source) => map(source, (string : string) => JSON.parse(string)),
        async (source) => {
          try {
            for await (const x of source) {
              inputHandler(id, x, peerId); //AB: Something is wonky with the parameters!
            }
          } catch (err) {
            error(`error in pipe`);
            processExpectedNetworkErrors(err, "setupConnection/pipe");
          }

          debug(`deleting entry for  ${id}`);                
          try {
            await _node.hangUp(peerId);
          } catch (err) {
            error(`error while hanging up pipe`);
            processExpectedNetworkErrors(err, "setupConnection/hang-up");
          }
          reissueUnacknowledged(id);
        }
  );

  stream.p = p; // Storing a reference to the pushable on the stream
                // We rely on the p2p library to keep track of streams
  debug(`Connection set up with ${id}`);
}

async function inputHandler(id, input, fromNodeId_) { //AB: fix parameters!!!
  let fromNodeId = fromNodeId_.toString();
  debug ("-- input handler");
  switch (input.messageType) {
    case (MessageType.SPAWN):
      if(_rt.remoteSpawnOK()) {
        debug("RECEIVED SPAWN");
        
        let x = await _rt.spawnFromRemote(input.message, fromNodeId);
        push_wrap(id, {
          messageType: MessageType.SPAWNOK,
          spawnNonce: input.spawnNonce,
          message: x
        });
      } // drop the message otherwise
      break;

    case (MessageType.SPAWNOK):
      debug ("SPAWN OK");
      let _cb = _spawnNonces[input.spawnNonce];
      if(_cb) {
        delete _spawnNonces[input.spawnNonce]; // cleanup
        _cb(null, input.message); // null means no errors
      } else {
        error("cannot find spawn callback");
      }
      break;

    case (MessageType.SEND):
      debug (`SEND from ${fromNodeId}`);
      _rt.receiveFromRemote(
        input.pid,
        input.message,
        fromNodeId
      );
      break;

    case (MessageType.WHEREIS): 
      debug("p2p whereis incoming request");
      let y = await _rt.whereisFromRemote(input.message);
      push_wrap(id, {
        messageType: MessageType.WHEREISOK, 
        whereisNonce : input.whereisNonce,
        message : y
      });
      debug("p2p whereis replied");
      break; 
        
    case (MessageType.WHEREISOK): 
      let _cbw = _whereisNonces[input.whereisNonce];
      if(_cbw) {
        delete _whereisNonces [input.whereisNonce]; // cleanup
        _cbw (null, input.message); // null means no errors
      } else {
        error("cannot find whereis callback");
      }
      break;

    case (MessageType.TEST):
      debug("TEST input");
      debug(input);
      break;
        
    default:
      debug(`received data ${input.toString('utf8').replace('\n', '')}`);
      break;
  }
}

// RELAY

export interface IHash {
  [details: string] : any;
}

let _relay_id = null;

//AB: only one counter?? And only one id??
let _keepAliveCounter = 0; //AB: can tag peers with "Keep_alive" in the peerstore (does it work?)
let _relayTable: IHash = {}; //AB: use in-built functionality instead?

async function keepAliveRelay(relay_addr:string) {
  let id = relay_addr.split('/').pop();
  debug(`relay id is ${id}`);
  let timeout = _KEEPALIVE;
  async function f() {
    try {            
      let p = _relayTable[id] ? _relayTable[id] : await dialRelay(relay_addr);
      p.push(`keep alive request ${_keepAliveCounter++}\n`); //AB: why do we do this???
      timeout = _KEEPALIVE;
    } catch (err) {
      timeout = timeout < 600e3 ? timeout * 2 : timeout // exponential backoff with 10 min limit            
      processExpectedNetworkErrors(err, "relay");
      error(`~~ error reaching the relay server; we will retry again in ${timeout/1000} seconds`);
    }             
    setTimeout(f, timeout);
  }
  f();
}

async function dialRelay(relay_addr) {
  debug(`dialing relay ${relay_addr}`);
  let id = relay_addr.split('/').pop();
  const relayId = peerIdFromString(id);

  await _node.peerStore.patch(relayId, {
    multiaddrs: [
      multiaddr(`${relay_addr}`)
    ],
    tags: {
      [KEEP_ALIVE]: {}
    }
  });

  debug (`Added relay address`);
  const conn = await _node.dial(relayId); //AB: necessary??
  debug (`Got relay connection`);
  const stream = await _node.dialProtocol(relayId, _RELAY_PROTOCOL);
  debug (`Got relay stream`);
  const peerId = conn.remotePeer;
  _relay_id = peerId.toString();
  debug(`~~ relay dialed, keep alive counter is ${_keepAliveCounter++}`);
  const p = pushable({ objectMode : true });
  
  pipe (stream.source, 
        async (source: any) => { 
          for await (const msg of source) {                   
            debug(`~~ relay says:${msg.toString().trim()}`);
          }                
        });
  pipe (p,
        (stream.sink as any));
  
  _relayTable[id] = p;
  return p;
}

// SEND

async function sendp2p(id : PeerId, procId, obj) {
  debug(`sendp2p`);

  let data = {
    messageType: MessageType.SEND,
    pid: procId,
    message: obj
  };
  
  push_wrap(id, data);
}

async function push_wrap(id: any, data: any) {
  debug(`push_wrap`);
  let connections = _node.getConnections(id);
  let needsToDial = true;
  let p = null;

  if(connections.length >= 1) {
    let connection = connections[0]; //AB: check all connections
    let streams = connection.streams;
    if(streams.length >= 1) {
      let stream = streams[0];
      p = (stream as any).p;
      needsToDial = p == undefined;
    } else {
      error("streams array empty");
      throw new Error //AB: What to do then? Can this happen?
    }
  }
  if(needsToDial) {
    debug("needs to dial");
    let stream = await dial(id.toString());
    debug("dialed to obtain stream");
    p = (stream as any).p;
  }
  
  debug (`push_wrap; stream obtained`);

  while (true) {
    try {
      debug(`push_wrap; pushing`);
      await p.push(data);
      debug(`push_wrap; data pushed into the stream`);
      break;
    } catch (err) {
      // the stream we have used is 
      // no good for whatever reason; 
      // most likely there are networking 
      // issues. we report the errors
      // and redial
      error(`push wrap error`);
      processExpectedNetworkErrors(err, "push_wrap");             
    }            
  }
}

// WHEREIS / SPAWN

let _whereisNonces  = {}; 
let _unacknowledged:any = {};

async function whereisp2p(id, str) {            
  let whereisNonce = uuidv4();

  function f() {
    push_wrap(id, {
      messageType : MessageType.WHEREIS,
      whereisNonce : whereisNonce,
      message : str 
    });
  }
  addUnacknowledged(id, whereisNonce, f);
  
  return new Promise((resolve, reject) => {        
    _whereisNonces[whereisNonce] = (err, data) => {
      if(err) {
        reject(err);
      } else { 
        removeUnacknowledged(id, whereisNonce);
        resolve(data);
      } 
    }
    debug("pushing whereis message");
    f();
  });
}

function addUnacknowledged(id, uuid, f) {
  if(!_unacknowledged[id]) { 
    _unacknowledged[id] = [];
  }
  _unacknowledged[id][uuid] = f;
}

function removeUnacknowledged(id, uuid) {
  delete _unacknowledged[id][uuid];
}

function reissueUnacknowledged(id:string) {
  for (let uuid in _unacknowledged[id]) {
    setImmediate(_unacknowledged[id][uuid]);
  }
}

let _spawnNonces = {};

async function spawnp2p(id, data) {
  const spawnNonce = uuidv4();

  return new Promise ((resolve, reject) => {
    _spawnNonces[spawnNonce] = (err, data) => {
      if(err) {
        reject(err);
      } else {
        resolve(data);
      }
    };
    push_wrap(id,  {
        messageType: MessageType.SPAWN,
        spawnNonce: spawnNonce,
        message: data
    });
  });
}

// HEALTH CHECK

function setupBlockingHealthChecker(period) {    
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

// ERROR HANDLING

function processExpectedNetworkErrors(err, source="source unknown") {    
    debug (`error source: ${source}`);
    if (err instanceof AggregateError) {
      for (const e of err.errors ) {        
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

// INTERFACE

export let p2p = {
  startp2p: (arg1, arg2) => {
    return startp2p(arg1, arg2)
  },
  spawnp2p: (arg1, arg2) => {
    return spawnp2p(arg1, arg2)
  },
  sendp2p: (arg1, arg2, arg3) => {
    return sendp2p(arg1, arg2, arg3)
  },
  whereisp2p: (arg1, arg2) => {
    return whereisp2p(arg1, arg2)
  },
  stopp2p: async () => {
    return await _node.stop()
  },
  processExpectedNetworkErrors: (arg1, arg2) => {
    return processExpectedNetworkErrors(arg1, arg2)
  }, 
}
