/*

The p2p runtime uses a Libp2p node to keep track of peers and connections.
A Publishable object is added to each stream to be used for sending messages.

A Publishable object is created in the following instances:

1. When we initate a connection to a node upon the first time, via
   node.dialProtocol because we are about to send a message (spawn or send) to
   that PeerId.

2. If we try to send a message and all open streams do not have a Publishable
   object that can be used.

3. When we receive a connection from some other in the callback passed to the
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

The messages themselves can be one of the six forms: 

1. SPAWN -- spawning on a remote node

2. SPAWNOK -- reply to the SPAWN (we did not have an analogue of this in express
   runtime because express was giving up the possibility of sending response to
   a given request, here we are doing it manually by maintaining a spawnNonce
   map).

3. SEND -- sending a message

4. TEST -- for testing/development purposes

5. WHEREIS -- asking for the address of a certain peer id

6. WHEREISOK -- reply to the WHEREIS

Note on the code below: the code below uses the libp2p framework, and is
partially grown out of the Chat example in that framework (to make sense of the
control flow transfer in this part of the runtime it may be helpful look up of
the libp2p).

*/

// AB: TODO:
// - discuss w/ Aslan
// - types... :(

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

const _PROTOCOL = "/troupe/1.0.0"; // Protocol for peers to talk to each other
const _RELAY_PROTOCOL = "/trouperelay/keepalive"; // Protocol for peers to talk to relays
const _HEALTHCHECKPERIOD = 5000; // How often the health check happens 2020-02-10; AA; this should be an option
const _KEEPALIVE = 5000; // Time-out for keep-alive messages to relay
const MessageType = {
  SPAWN: 0,
  SPAWNOK: 1,
  SEND: 2,
  TEST: 3,
  WHEREIS: 4,
  WHEREISOK: 5
};

// SET-UP

let _node: Libp2p = null; // The libp2p node this peer uses
let _rt = null; // The runtime object

let bootstrappers = [ 
  '/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmbLHAnMoJPWSCR5Zhtx6BHJX9KiKNN6tpvbUcqanj75Nb',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmZa1sAxajnQjVM8WjWXoMbmPd7NsWhfKsPkErzpm9wGkp',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmQCU2EcMqAqQPR2i9bChDtGNJchTbq5TbXJJ16u19uLTa',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmcZf59bWwK5XFi76CZX8cbJ4BhTzzA3gU1ZjYZcYW3dwt',
  '/ip4/134.122.54.216/tcp/5555/p2p/QmcQpBNGULxRC3QmvxVGXSw8BarpMvdADYvFtmvKAL5QMe',
];
let bootstrappers2 = p2pconfig.known_nodes.map((obj) => `${obj.ip}/${obj.nodeid}`);
//AB: bootstrap known_nodes from config?? Make findNode easier

/**
 * Start the libp2p node that this peer will use.
 * Also sets up the event queue block checker and
 * the connections to relays.
 */
async function startp2p(nodeId, rt: any): Promise<PeerId> {
  // Load or create a peer id
  let id : PeerId = await obtainPeerId(nodeId);

  // Create the libp2p node
  try {
    let nodeListener: Libp2p = await createLibp2p({
      peerId: id,
      addresses: {
        listen: [`/ip4/0.0.0.0/tcp/${__port}`]
      },
      connectionManager : {
        maxConnections: Infinity,
        minConnections: 0 //AB: What are good numbers here?
      }
    });
  
    // Save the libp2p node and runtime objects
    _node = nodeListener;
    _rt = rt;

  } catch (err) {
    error(`Something wrong while creating Libp2p node: ${err}`);
    throw err;
  }
  
  // When a peer dials using the Troupe protocol handle the connection
  await _node.handle(_PROTOCOL, async ({ connection, stream }) => {
    debug(`Handling protocol dial from id: ${connection.remotePeer}`);
    setupConnection(connection.remotePeer, stream);
  });

  // When a node is discovered report it on the debug logger
  _node.addEventListener('peer:discovery', async (evt) => {
    const peerInfo = evt.detail;
    debug(`Discovered: ${peerInfo.id.toString()}`);
  });

  // When a node is connected to report it on the debug logger
  _node.addEventListener('peer:connect', (evt) => {
    const peerId = evt.detail;
    debug(`Connection established to: ${peerId.toString()}`);
  });

  // When a node is disconnected from report it on the debug logger
  // If it is a relay, delete it from the relay table
  _node.addEventListener('peer:disconnect', (evt) => {
    let id = evt.detail;
    debug (`Disconnect from ${id}`);
    if (_relayTable[id.toString()]) {
      debug (`Deleting relay table entry`);
      delete _relayTable[id.toString()];
    }            
  });
  
  // When new addresses are added report it on the debug logger
  _node.addEventListener('self:peer:update', (_) => {
    debug(`Advertising with following addresses:`);
    _node.getMultiaddrs().forEach(m => debug(m.toString()));
  })

  debug("Libp2p node started");
  debug(`This node's id is ${id.toString()}`);

  // Set-up checking if the event queue is blocked
  setupBlockingHealthChecker(_HEALTHCHECKPERIOD);

  /*for (let relay_addr of p2pconfig.relays ) { //AB: update relay addresses in p2pconfig
    keepAliveRelay(relay_addr);
  }*/ //AB: update relay addresses in p2pconfig
  // Make sure relays are dialed and the connections are kept live
  keepAliveRelay("/ip4/134.209.92.133/tcp/5555/ws/p2p/12D3KooWShh9qmeS1UEgwWpjAsrjsigu8UGh8DRKyx1UG6HeHzjf");

  return id;
}

/**
 * Create the libp2p node that this peer will use.
 */
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

/**
 * Obtain this node's peer id.
 * Create it from a JSON object if possible,
 * otherwise generate a fresh one.
 */
async function obtainPeerId(nodeId): Promise<PeerId> {    
  let id: PeerId = null;
  if(nodeId) {
    // Load the id from a JSON file if possible
    try {
      id = await createFromJSON(nodeId);
      debug(`Loaded id from file: ${id.toString()}`);
    } catch (err) {
      error(`Error creating peer id from json: ${err}`);
      throw err;    
    }
  } else {
    // Otherwise create a fresh id
    try {
      debug("Creating new peer id...");
      id = await createEd25519PeerId();
      debug("Created new peer id");
    } catch (err) {
      error(`Error creating new peer id: ${err}`);
      throw err;
    }
  }

  return id;
}

// DIAL

/**
 * Dial the node `id` using the Troupe protocol.
 * First find addresses to use, then attempt to dial.
 * Give up if more than 10 attempts have failed.
 */
function dial(id: PeerId) {                 
  let i = 0;      
  let timeout = 2000;      
  return new Promise((resolve, reject) => {
    async function tryDialing() {
      try {
        // Add addresses to the peerStore
        await getPeerInfo(id);

        // Dial using the Troupe protocol
        debug(`Trying to dial ${id}, attempt number ${i}`);
        const stream = await _node.dialProtocol(id, _PROTOCOL);
        debug("Dial successful");

        // Handle inputs and outputs
        setupConnection (id, stream);

        resolve(stream);
      } catch (err) {
        processExpectedNetworkErrors (err, "dial");

        // if the error is suppressed we move on to trying 10 times with exponential backoff
        // 2020-02-10; AA: TODO: this code has a hardcoded constant 
        if(i <= 10) {
          debug(`Dial failed, we retry in ${timeout/1000} seconds`);
          debug(err);
          setTimeout(tryDialing, timeout);
          i++;
          timeout *= 2;
        } else {
          error(`Giving up on dialing ${id}: ${err}`);
          reject(err);
        } 
      }
    }
    tryDialing();                
  });
}

/**
 * Tries to find an address to use for a node.
 * Checks the known nodes from p2pconfig, the peerStore,
 * peerRouting and using a relay.
 */
async function getPeerInfo(id: PeerId): Promise<void> {
  let knownNodes = p2pconfig.known_nodes;
  debug(`Checking whether node is already known`);

  // Check whether the node is known in p2pconfig
  for(let ni of knownNodes) {
    if(ni.nodeid == id.toString()) {
      // Found a known node!
      await _node.peerStore.merge(id, {
        multiaddrs: [
          multiaddr(`${ni.ip}`)
        ]
      });
      debug(`Node ${ni.nodeid} will be contacted directly via IP: ${ni.ip}`);
      return;
    }
  }
  
  let needsToFind = true;

  // Check whether the node is known from previously
  // and has an address
  if(await _node.peerStore.has(id)) {
    try {
      let foundPeer = await _node.peerStore.get(id);
      if(foundPeer.addresses.length != 0) {
        debug("Peer info is in the store");
        needsToFind = false;
      }
    } catch (err) {
      error(`Error in getPeerInfo / peerStore.get: ${err}`);
      throw err;
    }
  }

  if(needsToFind) {
    // The node is not known or has no address
    debug("The node is not known; using peerRouting");
    await getPeerInfoWithPeerRouting(id);
  }

  if(_relayId) { //AB: use all relays as below!
    // Try to contact the node through a relay
    await _node.peerStore.merge(id, {
      multiaddrs: [
        multiaddr(`/p2p/${_relayId}/p2p-circuit/p2p/${id.toString()}`)
      ]
    });
  }
  // for (let i = 0; i < p2pconfig.relays.length; i++  ) {
  //     pi.multiaddrs.add( multiaddr(`${p2pconfig.relays[i]}/p2p-circuit/p2p/${id}`))
  // }
}

/**
 * Tries to find an address to use for a node through peerRouting.
 * Tries 6 times, then gives up.
 */
async function getPeerInfoWithPeerRouting(id: PeerId) : Promise<void> {
  return new Promise ((resolve, _) => {
    let n_attempts = 0;
    async function tryFindPeer() {
      try {
        // Try to find the node, but only spend 1 second on it
        debug(`Calling peerRouting.findPeer ${id}`);
        const peerInfo = await _node.peerRouting.findPeer(id, {signal : AbortSignal.timeout(1000)});
        debug ("findPeer returned");

        // Add the found address
        await _node.peerStore.merge(id, {
          multiaddrs: 
            peerInfo.multiaddrs
        });
        debug("Added multiaddr to store");

        resolve();
      } catch (err) {
        debug(`tryFindPeer exception`);

        if(err instanceof AggregateError) {
          for(let e of err.errors) {
            debug(`Find peer error with code: ${e}, ${e.code}`);
          }
        } else {
          debug(`Find peer error: ${err.toString()}`);
        }

        // Increase the attempts
        //AB: why only when nPeers > 0?
        if(nPeers() > 0) {
          n_attempts++;
        }
        // Try 6 times and then give up
        if(n_attempts > 5) {
          debug(`Giving up on peerRouting`);
          resolve();
        } else {
          debug(`tryFindPeer: attempt ${n_attempts} failed with ${nPeers()} nodes connected`);
          setTimeout(tryFindPeer, 500);
        }
      }
    }
    tryFindPeer();
  });
}

/**
 * Returns how many peers we have a connection to.
 */
function nPeers(): number {
  return _node.getPeers().length;
}

// CONNECTION SET-UP

/**
 * Sets up the connection with a new peer with `peerId`.
 * Ensures that messages that are sent and
 * received are marshalled correctly and
 * passes any input to the input handler.
 */
function setupConnection(peerId: PeerId, stream): void {        
  let id: string = peerId.toString();
  debug(`setupConnection with ${id}`);
  const p = pushable({ objectMode : true });

  // Setup the pipe to send and receive messages
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
            for await (const message of source) {
              // Send any input to the input handler
              inputHandler(id, message);
            }
          } catch (err) {
            processExpectedNetworkErrors(err, "setupConnection/pipe");
          }

          // Hangs up when the connection closes
          debug(`Hanging up connection to ${id}`);                
          try {
            await _node.hangUp(peerId);
          } catch (err) {
            processExpectedNetworkErrors(err, "setupConnection/hang-up");
          }
          // Resends any unacknowledged WHEREIS requests for this peer
          reissueUnacknowledged(id);
        }
  );

  stream.p = p; // Storing a reference to the pushable on the stream
                // We rely on the p2p library to keep track of streams
  debug(`Connection set up with ${id}`);
}

/**
 * Handles the different input types
 * - SPAWN: Checks whether remote spawn are allowed,
 *          informs the runtime and replies SPAWNOK.
 * - SPAWNOK: Gives the message to the call-back.
 * - SEND: Passes the message to the runtime.
 * - WHEREIS: Asks the runtime where the peer is,
 *            and replies with WHEREISOK.
 * - WHEREISOK: Gives the message to the call-back.
 * - TEST / other: Writes the input on the debug logger.
 */
async function inputHandler(id, input) {
  debug("Input handler");
  switch (input.messageType) {
    case (MessageType.SPAWN):
      // Check if spawning is allowed
      if(_rt.remoteSpawnOK()) {
        debug("Received SPAWN");
        
        // Inform the runtime
        let runtimeAnswer = await _rt.spawnFromRemote(input.message, id);

        // Reply with SPAWNOK
        pushWrap(id, {
          messageType: MessageType.SPAWNOK,
          spawnNonce: input.spawnNonce,
          message: runtimeAnswer
        });
        debug("SPAWN replied");
      } // Drop the message otherwise
      break;

    case (MessageType.SPAWNOK):
      debug ("Received SPAWN OK");
      // Find the call-back and give the message
      // Otherwise report an error
      let _cb = _spawnNonces[input.spawnNonce];
      if(_cb) {
        delete _spawnNonces[input.spawnNonce]; // Clean-up
        _cb(null, input.message); // null means no errors
      } else {
        error("Cannot find SPAWN callback");
      }
      break;

    case (MessageType.SEND):
      debug (`Received SEND from ${id}`);
      // Pass the message to the runtime
      _rt.receiveFromRemote(
        input.pid,
        input.message,
        id
      );
      break;

    case (MessageType.WHEREIS): 
      debug("Received WHEREIS");
      // Get the runtime to find the peer
      let runtimeAnswer = await _rt.whereisFromRemote(input.message);

     // Reply with WHEREISOK
      pushWrap(id, {
        messageType: MessageType.WHEREISOK, 
        whereisNonce : input.whereisNonce,
        message : runtimeAnswer
      });

      debug("WHEREIS replied");
      break; 
        
    case (MessageType.WHEREISOK):
      debug("Received WHEREISOK");
      // Find the call-back and give the message
      // Otherwise report an error
      let _cbw = _whereisNonces[input.whereisNonce];
      if(_cbw) {
        delete _whereisNonces [input.whereisNonce]; // Clean-up
        _cbw(null, input.message); // null means no errors
      } else {
        error("Cannot find WHEREIS callback");
      }
      break;

    case (MessageType.TEST):
      debug("Received TEST");
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

//AB: only one counter?? And only one id??
let _relayId = null;
let _keepAliveCounter = 0;
let _relayTable: IHash = {}; //AB: use in-built functionality instead?

/**
 * Send keep-alive messages to a relay at `relayAddr`.
 * If a message fails, do an exponential backoff
 * on the timeout for new tries.
 */
async function keepAliveRelay(relayAddr: string) {
  let id = relayAddr.split('/').pop();
  debug(`Relay id is ${id}`);

  let timeout = _KEEPALIVE;
  async function keepAlive() {
    try {
      // Push a keep-alive message to the relay.
      // If a pushable is not available, dial the relay.
      let p = _relayTable[id] ? _relayTable[id] : await dialRelay(relayAddr);
      p.push(`Keep alive request ${_keepAliveCounter++}\n`); //AB: why do we do this? What should the result be?
      // Reset the timeout once we are connected to the relay.
      timeout = _KEEPALIVE;
    } catch (err) {
      // Exponential backoff with 10 min limit.
      timeout = timeout < 600e3 ? timeout * 2 : timeout 
      processExpectedNetworkErrors(err, "relay");
      debug(`Error reaching the relay server; we will retry again in ${timeout/1000} seconds`);
    }             
    setTimeout(keepAlive, timeout);
  }
  keepAlive();
}

/**
 * Dials the relay at `relayAddr`
 * and returns the pushable for the relay.
 * Also tags the relay with "keep alive" in the peerStore.
 */
async function dialRelay(relayAddr: string) {
  debug(`Dialing relay at ${relayAddr}`);
  let id = relayAddr.split('/').pop();
  const relayId: PeerId = peerIdFromString(id);

  // Add the address to the peerStore
  // Tag the relay with "keep alive"
  await _node.peerStore.merge(relayId, {
    multiaddrs: [
      multiaddr(`${relayAddr}`)
    ],
    tags: {
      [KEEP_ALIVE]: {}
    }
  });

  // Dial the relay
  debug(`Added relay address`);
  const stream = await _node.dialProtocol(relayId, _RELAY_PROTOCOL);
  debug(`Got relay stream`);
  _relayId = id;
  debug(`Relay dialed, keep alive counter is ${_keepAliveCounter++}`);

  const p = pushable({ objectMode : true });
  // Set up receiving messages from the relay
  pipe (stream.source, 
        async (source: any) => { 
          for await (const msg of source) {                   
            debug(`Relay says:${msg.toString().trim()}`);
          }                
        });
  // Set up sending messages to the relay
  pipe (p,
        (stream.sink as any));
  
  // Save and return the pushable
  _relayTable[id] = p;
  return p;
}

// SEND

/**
 * Handles a send request to peer `id`.
 * Just pushes a SEND message.
 */
async function sendp2p(id: string, procId, obj) {
  debug(`sendp2p`);

  let data = {
    messageType: MessageType.SEND,
    pid: procId,
    message: obj
  };
  
  let peerId = peerIdFromString(id);
  debug("Pushing SEND message");
  pushWrap(peerId, data);
}

/**
 * Pushes `data` to a connection with `id`.
 * First finds a pushable on a connection with `id`.
 * Id this does not exists, dials `id`.
 * Then pushes the data.
 * Continues until the data is successfully pushed.
 */
async function pushWrap(id: PeerId, data: any) {
  while(true) {
    debug(`push wrap`);
    let connections = _node.getConnections(id);
    let needsToDial = true;
    let p = null;

    // Runs through all existing connections with the peer
    // and checks whether their streams have a pushable
    break_loop:
    for(const connection of connections) {
      let streams = connection.streams;
      for(const stream of streams) {
        p = (stream as any).p;
        needsToDial = (p == undefined);

        if(!needsToDial) {
          break break_loop;
        }
      }
    }

    try {
      // If no pushable was found, dial the peer
      if(needsToDial) {
        debug("Needs to dial");
        let stream = await dial(id);
        debug("Dialed to obtain stream");
        p = (stream as any).p;
      }
      
      // A pushable has been found, data can be pushed
      debug (`Push wrap; stream obtained; pushing`);
      await p.push(data);
      debug(`Push wrap; data pushed into the stream`);
      break;
    } catch (err) {
      // The pushable we have used is no good for whatever reason; 
      // most likely there are networking issues.
      // We report the errors and redial
      processExpectedNetworkErrors(err, "push wrap");             
    }
  }
}

// WHEREIS / SPAWN

let _whereisNonces = {}; // Stores call-backs for WHEREIS requests
let _unacknowledged: any = {}; // Keeps track of unacknowledged WHEREIS requests

/**
 * Handles a where-is request of peer `id`.
 * Creates a nonce which gives the result in the where-is table.
 * Also sets the request as unacknowledged.
 * Then pushes a WHEREIS message.
 */
async function whereisp2p(id: string, data: any) {
  debug("whereisp2p");

  // Create a nonce
  let whereisNonce = uuidv4();

  function sendMessage() {
    let peerId = peerIdFromString(id);
    pushWrap(peerId, {
      messageType : MessageType.WHEREIS,
      whereisNonce : whereisNonce,
      message : data 
    });
  }
  // Set the request as unacknowledged
  addUnacknowledged(id.toString(), whereisNonce, sendMessage);
  
  return new Promise((resolve, reject) => {
    // Return the error or result when an answer comes in
    _whereisNonces[whereisNonce] = (err, result) => {
      if(err) {
        reject(err);
      } else {
        // Only remoce the unacknowledged status if the request succeeds
        removeUnacknowledged(id.toString(), whereisNonce);
        resolve(result);
      } 
    }

    // Push the WHEREIS message
    debug("Pushing WHEREIS message");
    sendMessage();
  });
}

/**
 * Add the function `f` as unacknowledged
 * WHEREIS request for `id` with nonce `uuid`.
 */
function addUnacknowledged(id: string, uuid, f) {
  if(!_unacknowledged[id]) { 
    _unacknowledged[id] = [];
  }
  _unacknowledged[id][uuid] = f;
}

/**
 * Remove the unacknowledged WHEREIS request for
 * `id` with nonce `uuid`.
 */
function removeUnacknowledged(id: string, uuid) {
  delete _unacknowledged[id][uuid];
}

/**
 * Rerun all unacknowledged WHEREIS requests for `id`.
 */
function reissueUnacknowledged(id: string) {
  for(let uuid in _unacknowledged[id]) {
    setImmediate(_unacknowledged[id][uuid]);
  }
}

let _spawnNonces = {}; // Stores call-backs for SPAWN requests

/**
 * Handles a spawn request at peer `id`.
 * Creates a nonce which gives the result in the spawn table.
 * Then pushes a SPAWN message to the receiving peer.
 */
async function spawnp2p(id: string, data: any) { //AB: why not use "unanknowledged" like for WHEREIS?
  debug("spawnp2p");
  
  // Create a nonce
  const spawnNonce = uuidv4();

  return new Promise ((resolve, reject) => {
    // Return the error or result when an answer comes in
    _spawnNonces[spawnNonce] = (err, result) => {
      if(err) {
        reject(err);
      } else {
        resolve(result);
      }
    };

    // Push the SPAWN message
    debug("Pushing SPAWN message");
    let peerId = peerIdFromString(id);
    pushWrap(peerId, {
        messageType: MessageType.SPAWN,
        spawnNonce: spawnNonce,
        message: data
    });
  });
}

// HEALTH CHECK

/**
 * Checks that the event queue does not get blocked.
 * The check is scheduled to run in `period` millisecond intervals.
 * If it takes much longer than that before the check runs,
 * this is reported, since it indicates blocking.
 */
function setupBlockingHealthChecker(period: number) {    
    let lastHealth: number = Date.now();
    let healthCounter = 0;
    let healthThreshold = Math.max(period * 1.25 , period + 50);
    // AA: 2020-02-10;
    // The event queue always has a fair bit of latency, so we adjust for 
    // the minimal expected latency here; the constant of 50 is an
    // empirically derived value, but needs to be critically reevaluated
    // as the system evolves

    function checkBlocking() {
      let now = Date.now()
      // check and report if it has been too long since the last health check
      // this could indicate that something is the event queue
      if(now - lastHealth > healthThreshold) {
        debug(`Potential blocking issue`);
        debug(`Health check ${healthCounter} took this long ${now - lastHealth}`);
      }
      lastHealth = now;
      healthCounter++;

      // Run the check periodically
      setTimeout(checkBlocking, period);
    }
    checkBlocking();
}

// ERROR HANDLING

/**
 * Breaks down aggregate errors to their components.
 * Any known errors are reported.
 * Any unknown errors are reported and thrown.
 */
function processExpectedNetworkErrors(err, source="unknown") {
    debug(`Error source: ${source}`);
    if(err instanceof AggregateError) {
      for(const e of err.errors ) {        
        processExpectedNetworkErrors (e, source)
      }
    } else {
      if(err.code) {
        switch (err.code) {
          case 'ENETUNREACH':
            debug(`${err.toString()}`)
            break;
          case 'ENOTFOUND':
            debug(`${err.toString()}`)
            break;
          case 'ECONNRESET':
            debug(`${err.toString()}`)
            break;
          case 'ERR_TRANSPORT_DIAL_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'ABORT_ERR':
            debug(`${err.toString()}`)
            break;
          case 'ECONNREFUSED':
            debug(`${err.toString()}`)
            break;
          case 'ERR_HOP_REQUEST_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'ERR_NO_DIAL_MULTIADDRS':
            debug(`${err.toString()}`)
            break;
          case 'ERR_ENCRYPTION_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'ERR_NO_VALID_ADDRESSES':
            debug(`${err.toString()}`)
            break;  
          case 'ERR_MPLEX_STREAM_RESET':
            debug(`${err.toString()}`)
            break;  

          default:
            error(`Unhandled error case with error code ${err.code}`)
            throw err;
        }
      } else {
        error(`Unhandled general error case ${err}`)
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
