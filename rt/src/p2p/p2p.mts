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

// IMPORTS

import type { PeerId } from '@libp2p/interface';
import { FaultTolerance } from '@libp2p/interface';
import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
import { tcp } from '@libp2p/tcp';
import { webSockets } from '@libp2p/websockets';
import { mplex } from '@libp2p/mplex';
import { yamux } from '@chainsafe/libp2p-yamux';
import { noise } from '@chainsafe/libp2p-noise';
import defaultsDeep from '@nodeutils/defaults-deep';
import { Libp2p, createLibp2p as create } from 'libp2p';
import { keys } from '@libp2p/crypto';
import { peerIdFromPrivateKey } from '@libp2p/peer-id';
import { peerIdFromString } from '@libp2p/peer-id';
import { bootstrap } from '@libp2p/bootstrap';
import { mdns } from '@libp2p/mdns';
import { pipe } from 'it-pipe';
import * as lp from 'it-length-prefixed';
import map from 'it-map';
import { fromString as uint8ArrayFromString } from 'uint8arrays/from-string';
import { toString as uint8ArrayToString } from 'uint8arrays/to-string';
import { pushable } from 'it-pushable';
import p2pconfig, { setCliRelays, getRelays } from './p2pconfig.mjs';
import { multiaddr } from '@multiformats/multiaddr';
import { identify } from '@libp2p/identify';
import { circuitRelayTransport } from '@libp2p/circuit-relay-v2';
import { ping } from '@libp2p/ping';
const KEEP_ALIVE = 'KEEP_ALIVE'; // Tag for keeping connections alive
import { Logger } from 'winston';
import {v4 as uuidv4} from 'uuid';
import { kadDHT } from '@libp2p/kad-dht';

// USER-FACING ERRORS

/**
 * Error class for P2P errors that should be displayed to users without stack traces.
 * Similar to CliValidationError for network-related user-facing errors.
 */
export class P2pUserError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'P2pUserError';
    }
}

// LOGGING AND DEBUGGING 

const argv = getCliArgs();

let logLevel = argv[TroupeCliArg.DebugP2p]? 'debug':'info';
let __port = argv[TroupeCliArg.Port] || 0;

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

const bootstrappers = [
  // libp2p bootstrap nodes
  // (from https://github.com/libp2p/js-libp2p/blob/b36ec7f24e477af21cec31effc086a6c611bf271/examples/discovery-mechanisms/README.md?plain=1#L60)
  '/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmbLHAnMoJPWSCR5Zhtx6BHJX9KiKNN6tpvbUcqanj75Nb',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmZa1sAxajnQjVM8WjWXoMbmPd7NsWhfKsPkErzpm9wGkp',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmQCU2EcMqAqQPR2i9bChDtGNJchTbq5TbXJJ16u19uLTa',
  '/dnsaddr/bootstrap.libp2p.io/p2p/QmcZf59bWwK5XFi76CZX8cbJ4BhTzzA3gU1ZjYZcYW3dwt'
];

/**
 * Start the libp2p node that this peer will use.
 * Also sets up the event queue block checker and
 * the connections to relays.
 */
async function startp2p(nodeId, rt: any): Promise<String> {
  // Set CLI relays if provided
  const cliRelays = argv[TroupeCliArg.Relay];
  if (cliRelays) {
    setCliRelays(Array.isArray(cliRelays) ? cliRelays : [cliRelays]);
  }

  // Load or create a private key
  let privateKey = await obtainPrivateKey(nodeId);
  let id: PeerId;

  // Create the libp2p node
  try {
    let nodeListener: Libp2p = await createLibp2p({
      privateKey: privateKey,
    });

    await nodeListener.start();
  
    // Save the libp2p node and runtime objects
    _node = nodeListener;
    _rt = rt;
    
    // Get the peer ID from the node
    id = nodeListener.peerId;

  } catch (err) {
    // Check if this is a relay-related address listening failure
    // UnsupportedListenAddressesError occurs when libp2p cannot listen on configured addresses,
    // typically because the relay server is unreachable (timeout)
    if (err.name === 'UnsupportedListenAddressesError' && err.message) {
      const relays = getRelays();
      // Check if any configured relay address appears in the error message
      const failedRelay = relays?.find(relay => err.message.includes(relay));
      if (failedRelay) {
        const message = `Relay server unreachable: ${failedRelay}\n` +
          `The relay server did not respond in time. Workarounds:\n` +
          `  --disable-relay             Disable relay functionality entirely\n` +
          `  --relay-fault-tolerance=no-fatal  Continue startup even if relay fails`;
        throw new P2pUserError(message);
      }
    }
    error(`Something wrong while creating Libp2p node: ${err}`);
    throw err;
  }
  
  // When a peer dials using the Troupe protocol, handle the connection
  // runOnLimitedConnection is required for circuit relay connections
  await _node.handle(_PROTOCOL, async (stream, connection) => {
    debug(`Handling protocol dial from id: ${connection.remotePeer}`);
    setupConnection(connection.remotePeer, stream);
  }, { runOnLimitedConnection: true });

  // When a node is discovered, save the address and report it on the debug logger
  _node.addEventListener('peer:discovery', async (evt) => {
    const peerInfo = evt.detail;
    await _node.peerStore.patch(peerInfo.id, {
      multiaddrs: peerInfo.multiaddrs
    });
    debug(`Discovered: ${peerInfo.id.toString()}`);
  });

  // When a node is connected to, report it on the debug logger
  _node.addEventListener('peer:connect', (evt) => {
    const peerId = evt.detail;
    debug(`Connection established to: ${peerId.toString()}`);
  });

  // When a node is disconnected from, report it on the debug logger
  _node.addEventListener('peer:disconnect', (evt) => {
    let id = evt.detail;
    debug(`Disconnect from ${id}`);
  });
  
  // When new addresses are added report it on the debug logger
  _node.addEventListener('self:peer:update', (_) => {
    debug(`Advertising with following addresses:`);
    _node.getMultiaddrs().forEach(m => debug(m.toString()));
  });

  debug("Libp2p node started");
  debug(`This node's id is ${id.toString()}`);

  // Set-up checking if the event queue is blocked
  setupBlockingHealthChecker(_HEALTHCHECKPERIOD);

  // Make sure the relay is dialed and the connections are kept live
  // To use more than one relay, make sure to dial them all
  // Skip if --no-relay is set
  const disableRelay = argv[TroupeCliArg.DisableRelay] || false;
  if (disableRelay) {
    debug("--disable-relay: Skipping relay connection setup");
  } else {
    const relays = getRelays();
    if (relays && relays.length > 0) {
      keepAliveRelay(relays[0]);
    } else {
      debug("No relay configured, skipping relay connection");
    }
  }

  return id.toString();
}

/**
 * Create the libp2p node that this peer will use.
 */
async function createLibp2p(_options) {
  const relayOnly = argv[TroupeCliArg.RelayOnly] || false;
  const noP2pCircuit = argv[TroupeCliArg.NoP2pCircuit] || false;
  const disableRelay = argv[TroupeCliArg.DisableRelay] || false;
  const relayFaultTolerance = argv[TroupeCliArg.RelayFaultTolerance] || 'fatal';

  // Determine fault tolerance setting for transport manager
  const faultTolerance = relayFaultTolerance === 'no-fatal'
    ? FaultTolerance.NO_FATAL
    : FaultTolerance.FATAL_ALL;

  // Build listen addresses
  const listenAddrs = [`/ip4/0.0.0.0/tcp/${__port}`];

  // For circuit relay v2, we need to specify the exact relay address to listen on.
  // Using just '/p2p-circuit' triggers relay discovery, but in relay-only mode
  // discovery is disabled. By specifying the full relay address with /p2p-circuit,
  // we tell the transport to make a reservation on that specific relay.
  // Skip all circuit relay addresses if --disable-relay is set
  if (!disableRelay && !noP2pCircuit) {
    const relays = getRelays();
    if (relays && relays.length > 0) {
      // Use specific relay addresses (e.g., /ip4/.../p2p/RELAYID/p2p-circuit)
      for (const relay of relays) {
        listenAddrs.push(`${relay}/p2p-circuit`);
        debug(`Adding circuit relay listen address: ${relay}/p2p-circuit`);
      }
    } else {
      // No relays configured - skip circuit relay listen address
      // Direct connectivity via TCP/mDNS will still work
      debug('No relays configured, skipping /p2p-circuit listen address');
    }
  } else if (disableRelay) {
    debug('--disable-relay: All relay functionality disabled');
  } else {
    debug('--no-p2p-circuit: Relay reservations disabled (for testing NO_RESERVATION handling)');
  }

  // Build transports list - exclude circuit relay transport if --disable-relay is set
  const transports = disableRelay
    ? [tcp(), webSockets()]
    : [tcp(), webSockets(), circuitRelayTransport({})];

  const defaults: any = {
    addresses: {
      listen: listenAddrs
    },
    connectionManager : {
      minConnections: 1,
      maxConnections: Infinity,
    },
    transportManager: {
      faultTolerance: faultTolerance
    },
    transports: transports,
    streamMuxers: [
      yamux(),
      // mplex is deprecated and can cause issues with circuit relay protobuf decoding
      // See: https://docs.libp2p.io/concepts/multiplex/mplex/
      // mplex()
    ],
    connectionEncrypters: [
      noise(),
    ],
    services: {
      ping: ping(),
      identify: identify(),
    },
  };

  // Only enable DHT, mDNS, and bootstrap discovery if not in relay-only mode
  if (relayOnly) {
    debug('Relay-only mode: DHT, mDNS, and bootstrap discovery disabled');
    defaults.peerDiscovery = [];
    // No DHT service in relay-only mode
  } else {
    defaults.peerDiscovery = [
      bootstrap({
        list: bootstrappers
      }),
      mdns(),
    ];
    defaults.services.dht = kadDHT({
      clientMode: false,  // Run as both client and server
      protocol: '/ipfs/kad/1.0.0'
    });
  }

  return create(defaultsDeep(_options, defaults));
}

/**
 * Obtain this node's private key.
 * Create it from a protobuf if possible,
 * otherwise generate a fresh one.
 */
async function obtainPrivateKey(nodeId): Promise<any> {    
  let privateKey: any = null;
  if(nodeId && nodeId.privKey) {
    // Load the private key from the nodeId object
    try {
      // Convert base64pad private key string to Uint8Array
      const privKeyBytes = Uint8Array.from(Buffer.from(nodeId.privKey.trim(), 'base64'));
      privateKey = await keys.privateKeyFromProtobuf(privKeyBytes);
      const id = await peerIdFromPrivateKey(privateKey);
      debug(`Loaded id from file: ${id.toString()}`);
    } catch (err) {
      error(`Error creating private key from protobuf: ${err}`);
      throw err;    
    }
  } else {
    // Otherwise create a fresh key pair
    try {
      debug("Creating new key pair...");
      privateKey = await keys.generateKeyPair('Ed25519');
      const id = await peerIdFromPrivateKey(privateKey);
      debug(`Created new id: ${id.toString()}`);
    } catch (err) {
      error(`Error creating key pair: ${err}`);
      throw err;
    }
  }

  return privateKey;
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
        // runOnLimitedConnection is required for circuit relay connections
        debug(`Trying to dial ${id}, attempt number ${i}`);
        const stream = await _node.dialProtocol(id, _PROTOCOL, { runOnLimitedConnection: true });
        debug("Dial successful");

        // Handle inputs and outputs
        setupConnection(id, stream);

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
 * Tries to find and add addresses to use for a node.
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
  
  let usePeerRouting = true;

  // Check whether the node is known from previously
  // and has an address
  if(await _node.peerStore.has(id)) {
    try {
      let foundPeer = await _node.peerStore.get(id);
      if(foundPeer.addresses.length != 0) {
        debug("Peer info is in the store");
        usePeerRouting = false;
      }
    } catch (err) {
      error(`Error in getPeerInfo / peerStore.get: ${err}`);
      throw err;
    }
  }

  if(usePeerRouting) {
    // The node is not known or has no address
    debug("The node is not known; using peerRouting");
    await getPeerInfoWithPeerRouting(id);
  }

  if(_relayId) {
    // Try to contact the node through a relay
    // To use several relays, cycle through them and add them all
    debug(`Adding circuit relay address for ${id}: /p2p/${_relayId}/p2p-circuit/p2p/${id.toString()}`);
    await _node.peerStore.merge(id, {
      multiaddrs: [
        multiaddr(`/p2p/${_relayId}/p2p-circuit/p2p/${id.toString()}`)
      ]
    });
  }
}

/**
 * Tries to find an address to use for a node through
 * peerRouting. Tries six times, then gives up.
 */
async function getPeerInfoWithPeerRouting(id: PeerId) : Promise<void> {
  return new Promise ((resolve, _) => {
    let n_attempts = 0;
    async function tryFindPeer() {
      try {
        // Try to find the node, but only spend 1 second on it
        debug(`Calling peerRouting.findPeer ${id}`);
        const peerInfo = await _node.peerRouting.findPeer(id, {signal : AbortSignal.timeout(2000)});
        debug("findPeer returned");

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

        // Increase the attempts used
        // Only if the node is connected to the network
        if(nPeers() > 0) {
          n_attempts++;
        }
        // Try six times and then give up
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
// A frame that is not valid JSON on the Troupe protocol is an expected
// adversarial input at the transport boundary: any peer that can reach the port
// can send arbitrary bytes, before any trust check or deserialization runs.
// Wrap the parse so the read pipe can drop the offending connection and keep the
// node serving, rather than let a raw SyntaxError reach the fatal default arm of
// processExpectedNetworkErrors and terminate the process.
class MalformedInboundFrame extends Error {
  constructor(peer: string, cause: unknown) {
    super(`malformed inbound frame from ${peer}: `
      + `${cause instanceof Error ? cause.message : String(cause)}`);
    this.name = 'MalformedInboundFrame';
  }
}

function parseInboundFrame(id: string, s: string): any {
  try {
    return JSON.parse(s);
  } catch (e) {
    throw new MalformedInboundFrame(id, e);
  }
}

function setupConnection(peerId: PeerId, stream): void {
  let id: string = peerId.toString();
  debug(`setupConnection with ${id}`);
  const p = pushable({ objectMode : true });

  // In libp2p v3, streams are AsyncIterable for reading and use .send() for writing
  // We need to set up separate read and write pipelines

  // Write pipeline: pushable -> encode -> send to stream
  pipe(
    p,
    (source) => map(source, (json) => JSON.stringify(json)),
    (source) => map(source, (string: string) => uint8ArrayFromString(string, 'utf8')),
    (source) => lp.encode(source),
    async (source) => {
      try {
        for await (const data of source) {
          stream.send(data);
        }
      } catch (err) {
        processExpectedNetworkErrors(err, "setupConnection/write-pipe");
      }
    }
  );

  // Read pipeline: stream -> decode -> handle messages
  pipe(
    stream,
    (source) => lp.decode(source),
    (source) => map(source, (buf) => uint8ArrayToString(buf.subarray())),
    (source) => map(source, (string: string) => parseInboundFrame(id, string)),
    async (source) => {
      try {
        for await (const message of source) {
          // Send any input to the input handler. inputHandler is async and is
          // intentionally not awaited here (messages are processed as they
          // arrive). Its handlers drop expected adversarial inputs at their own
          // boundary; anything that still rejects is unexpected, so log it with
          // peer context and let it surface (the node's fail-fast policy) rather
          // than have it reach the process-level handler with no context.
          inputHandler(id, message).catch(err => {
            error(`Unexpected error handling message from ${id}: ${err?.stack ?? err}`);
            throw err;
          });
        }
      } catch (err) {
        if (err instanceof MalformedInboundFrame) {
          // Expected: a peer spoke non-JSON on the Troupe protocol. Log it and
          // fall through to hang up this connection below; the node keeps
          // serving every other peer. The fatal default arm of
          // processExpectedNetworkErrors is reserved for the unexpected.
          info(`Dropping connection to ${id}: ${err.message}`);
        } else {
          processExpectedNetworkErrors(err, "setupConnection/read-pipe");
        }
      }

      // Hangs up when the connection closes
      debug(`Hanging up connection to ${id}`);
      try {
        await _node.hangUp(peerId);
      } catch (err) {
        processExpectedNetworkErrors(err, "setupConnection/hang-up");
      }

      // Resends any unacknowledged WHEREIS and SPAWN requests for this peer
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
      // Drop the message otherwise
      if(_rt.remoteSpawnOK()) {
        debug("Received SPAWN");

        if(receivedSpawnNonces[input.spawnNonce]) {
          // This is an already seen spawn request.
          // Look up the reply and resend without spawning again
          debug("This spawn was already received; replying again without spawning");
          let cachedAnswer = receivedSpawnNonces[input.spawnNonce];

          // Reply with SPAWNOK and return
          pushWrap(id, {
            messageType: MessageType.SPAWNOK,
            spawnNonce: input.spawnNonce,
            message: cachedAnswer
          });
          return;
        }

        // Inform the runtime
        let runtimeAnswer = await _rt.spawnFromRemote(input.message, id);

        // Reply with SPAWNOK
        pushWrap(id, {
          messageType: MessageType.SPAWNOK,
          spawnNonce: input.spawnNonce,
          message: runtimeAnswer
        });
        debug("SPAWN replied");

        // Save the nonce and the answer for 10 minutes
        // in case we get the same request again
        receivedSpawnNonces[input.spawnNonce] = runtimeAnswer;
        function deleteSpawnNonce() {
          delete receivedSpawnNonces[input.spawnNonce];
        }
        setTimeout(deleteSpawnNonce, 600000);
      }
      break;

    case (MessageType.SPAWNOK):
      debug("Received SPAWN OK");
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
      debug(`Received SEND from ${id}`);
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
      let runtimeAnswer = await _rt.whereisFromRemote(input.message, id);

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

let _relayId = null; // The id for the relay
let _keepAliveCounter = 0; // The number of times "keep-alive" has been sent to the relay
// To use more than one relay, these should be kept in a table

/**
 * Send keep-alive messages to a relay at `relayAddr`.
 * If a message fails, do an exponential backoff
 * on the timeout for new tries.
 */
async function keepAliveRelay(relayAddr: string) {
  let id = relayAddr.split('/').pop();
  debug(`Relay id is ${id}`);

  // In circuit relay v2, we just need to dial the relay once
  // The connection will be maintained automatically by libp2p
  try {
    await dialRelay(relayAddr);
    debug(`Successfully connected to relay ${id}`);
  } catch (err) {
    error(`Failed to connect to relay: ${err}`);
    processExpectedNetworkErrors(err, "relay");
    
    // Retry after a delay if connection fails
    setTimeout(() => keepAliveRelay(relayAddr), 30000);
  }
}

/**
 * Dials the relay at `relayAddr`
 * and returns the pushable for the relay.
 * Also tags the relay with "keep alive" in the peerStore.
 */
async function dialRelay(relayAddr: string) {
  try {
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

    // Dial the relay - in circuit relay v2, we just dial without a specific protocol
    debug(`Added relay address`);
    const connection = await _node.dial(relayId);
    debug(`Relay dialed`);
    _relayId = id;

    // In circuit relay v2, the reservation is made automatically when we dial the relay
    // because we have /p2p-circuit in our listen addresses (configured in createLibp2p).
    // The circuitRelayTransport handles the HOP RESERVE protocol internally.
    // Log the relay circuit addresses we're now reachable at.
    const relayAddrs = _node.getMultiaddrs().filter(m => m.toString().includes('p2p-circuit'));
    if (relayAddrs.length > 0) {
      debug(`Relay reservation established - now reachable through relay ${id}`);
      relayAddrs.forEach(addr => debug(`Relay address: ${addr.toString()}`));
    } else {
      debug(`Relay dialed but no circuit addresses yet - reservation may be pending`);
    }

    debug(`Relay connected, keep alive counter is ${_keepAliveCounter++}`);
    return null;
  } catch (err) {
    processExpectedNetworkErrors (err, "dial relay");
  }
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
 * First finds a pushable on a connection with
 * `id`, then pushes the data.
 * Continues until the data is successfully pushed.
 */
async function pushWrap(id: PeerId, data: any) {
  while(true) {
    debug(`pushWrap`);
    let p = await getPushable(id);
    
    try {
      if(p) {
        // A pushable has been found, data can be pushed
        debug(`Stream obtained; pushing`);
        await p.push(data);
        debug(`Data pushed into the stream`);
        break;
      } else {
        debug("Pushable found was null; re-trying");
      }
    } catch (err) {
      // The pushable we have used is no good for whatever reason; 
      // most likely there are networking issues.
      // We report the errors and redial
      processExpectedNetworkErrors(err, "pushWrap");             
    }
  }
}

/**
 * Finds a pushable to node `id` by checking all
 * existing connections with the node. If no 
 * existing pushable is found, then dials the node.
 */
async function getPushable(id: PeerId, relayAddr=null) {
  debug("getPushable");
  let connections = _node.getConnections(id);
  let needsToDial = true;
  let p = null;

  // Runs through all existing connections with the peer
  // and checks whether their streams have a pushable
  for(const connection of connections) {
    let streams = connection.streams;
    for(const stream of streams) {
      p = (stream as any).p;
      needsToDial = (p == undefined);

      if(!needsToDial) {
        debug("Found existing pushable");
        return p;
      }
    }
  }

  // If no pushable was found, dial the peer
  if(needsToDial) {
    debug("Needs to dial");
    let stream = null;
    if(relayAddr) {
      stream = await dialRelay(relayAddr);
    } else {
      stream = await dial(id);
    }
    // If the dial fails, the stream is null
    if(stream) {
      debug("Dialed to obtain stream");
      p = (stream as any).p;
    } else {
      debug("Could not obtain stream through dial");
    }
  }

  return p;
}

// WHEREIS / SPAWN

let _whereisNonces = {}; // Stores call-backs for WHEREIS requests
let _spawnNonces = {}; // Stores call-backs for SPAWN requests
let receivedSpawnNonces = {}; // Stores received SPAWN nonces and the runtime answer for their reply
                              // These are stored for 10 minutes in case the SPAWNOK disappeared
let _unacknowledged: any = {}; // Keeps track of unacknowledged WHEREIS and SPAWN requests

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

  // Set the WHEREIS request as unacknowledged
  addUnacknowledged(id.toString(), whereisNonce, sendMessage);
  
  return new Promise((resolve, reject) => {
    // Return the error or result when an answer comes in
    _whereisNonces[whereisNonce] = (err, result) => {
      if(err) {
        reject(err);
      } else {
        // Only remove the unacknowledged status if the request succeeds
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
 * Handles a spawn request at peer `id`.
 * Creates a nonce which gives the result in the spawn table.
 * Then pushes a SPAWN message to the receiving peer.
 */
async function spawnp2p(id: string, data: any) {
  debug("spawnp2p");
  
  // Create a nonce
  const spawnNonce = uuidv4();

  function sendMessage() {
    let peerId = peerIdFromString(id);
    pushWrap(peerId, {
      messageType : MessageType.SPAWN,
      spawnNonce : spawnNonce,
      message : data 
    });
  }

  // Set the SPAWN request as unacknowledged
  addUnacknowledged(id.toString(), spawnNonce, sendMessage);

  return new Promise ((resolve, reject) => {
    // Return the error or result when an answer comes in
    _spawnNonces[spawnNonce] = (err, result) => {
      if(err) {
        reject(err);
      } else {
        // Only remove the unacknowledged status if the request succeeds
        removeUnacknowledged(id.toString(), spawnNonce);
        resolve(result);
      }
    };

    // Push the SPAWN message
    debug("Pushing SPAWN message");
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
    } else if(err && err.constructor && err.constructor.name === 'ErrorEvent') {
      // Handle ErrorEvent from native Node.js WebSocket (via undici)
      // ErrorEvent doesn't have name/code properties, just a type property
      // This occurs when WebSocket connections fail (e.g., connection refused, network error)
      const target = err.target;
      const url = target && target[Symbol.for('nodejs.url')] ? target[Symbol.for('nodejs.url')] : 'unknown';
      error(`WebSocket connection failed to ${url}: ${err.message || 'connection error'}`);
      // Treat as a recoverable network error - don't throw
    } else {
      if(err.name || err.code) {
        const errorId = err.name || err.code;
        switch (errorId) {
          case 'NetworkUnreachableError':
          case 'ENETUNREACH':
          case 'EHOSTUNREACH':
            debug(`${err.toString()}`)
            break;
          case 'NotFoundError':
          case 'ENOTFOUND':
            debug(`${err.toString()}`)
            break;
          case 'ConnectionResetError':
          case 'ECONNRESET':
            debug(`${err.toString()}`)
            break;
          case 'TransportDialFailedError':
          case 'ERR_TRANSPORT_DIAL_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'AbortError':
          case 'ABORT_ERR':
            debug(`${err.toString()}`)
            break;
          case 'ConnectionRefusedError':
          case 'ECONNREFUSED':
            debug(`${err.toString()}`)
            break;
          case 'HopRequestFailedError':
          case 'ERR_HOP_REQUEST_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'NoDialMultiaddrsError':
          case 'ERR_NO_DIAL_MULTIADDRS':
            debug(`${err.toString()}`)
            break;
          case 'EncryptionFailedError':
          case 'ERR_ENCRYPTION_FAILED':
            debug(`${err.toString()}`)
            break;
          case 'NoValidAddressesError':
          case 'ERR_NO_VALID_ADDRESSES':
            debug(`${err.toString()}`)
            break;
          case 'StreamResetError':
          case 'ERR_MPLEX_STREAM_RESET':
            debug(`${err.toString()}`)
            break;
          case 'TimeoutError':
          case 'ERR_TIMEOUT':
            debug(`${err.toString()}`);
            break;
          case 'InvalidMessageError':
            // Check if this is a relay-related error (NO_RESERVATION, etc.)
            if (err.message && err.message.includes('NO_RESERVATION')) {
              debug(`Relay reservation not found: ${err.toString()}`);
            } else if (err.message && err.message.includes('RESOURCE_LIMIT_EXCEEDED')) {
              debug(`Relay resource limit exceeded: ${err.toString()}`);
            } else if (err.message && err.message.includes('PERMISSION_DENIED')) {
              debug(`Relay permission denied: ${err.toString()}`);
            } else {
              // Unknown InvalidMessageError - log as error but don't throw
              error(`InvalidMessageError: ${err.toString()}`);
            }
            break;
          case 'ReservationRefusedError':
          case 'ERR_RESERVATION_REFUSED':
            debug(`Relay reservation refused: ${err.toString()}`);
            break;
          case 'ConnectionFailedError':
          case 'ERR_CONNECTION_FAILED':
            debug(`Connection failed: ${err.toString()}`);
            break;
      

          default:
            error(`Unhandled error case with error identifier ${errorId}`)
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
    // End all pushables before stopping to allow _node.stop() to complete.
    // Without ending pushables, the write pipelines block indefinitely.
    for (const connection of _node.getConnections()) {
      for (const stream of connection.streams) {
        const p = (stream as any).p;
        if (p) p.end();
      }
    }
    return await _node.stop()
  },
  processExpectedNetworkErrors: (arg1, arg2) => {
    return processExpectedNetworkErrors(arg1, arg2)
  }, 
}
