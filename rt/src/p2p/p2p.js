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



const logger = require('../logger.js').mkLogger('p2p',logLevel);
const info = x => logger.info(x)
const debug = x => logger.debug(x)
const error = x => logger.error(x);


// util 

function promisify01 (f) {
    return new Promise ( (resolve, reject) => {
        f ((err, x) => {
            if (err) { reject (err) } else { resolve }
        })
    });
}


// 
const p2pconfig = require('./p2pconfig.js')
const pull = require('pull-stream')
const PeerId = require("peer-id")
const PeerInfo = require("peer-info")
const Node = require("./libp2p-bundle")
const multiaddr = require('multiaddr')



// const ffp = require('find-free-port');

const Pushable = require('pull-pushable')
const uuidv4 = require('uuid/v4');

// const ProgressBar = require('progress');

// 2018-09-20: aa: promisification
const { promisify } = require ('util')
const createFromJSON = promisify (PeerId.createFromJSON);
const createPeerId = promisify (PeerId.create);


const MessageType = {
    SPAWN: 0,
    SPAWNOK: 1,
    SEND: 2,
    TEST: 3,
    WHEREIS: 4,
    WHEREISOK: 5    
}



async function loadPeerId(nodeId) {
    let id = null;
    if (nodeId) {
        try {
            id = await createFromJSON(nodeId);
            debug(`Loaded id from file: ${id.toB58String()}`);
        } catch (err ) {
            logger.error("error creating peer id from json");
            throw err        
        }
    } else {
        try {
            id = await createPeerId()
            debug("Created new pair id");
        } catch (err) {
            logger.error("error creating new peer id");
            throw err;
        }
    }
    let _peerInfo = new PeerInfo(id)
    
    
    // _peerInfo.multiaddrs.add(multiaddr('/p2p-websocket-star'));
    _peerInfo.multiaddrs.add(p2pconfig.rendezvousServer);    
    _peerInfo.multiaddrs.add(`/ip4/0.0.0.0/tcp/0`); 
    return _peerInfo;
}

function TroupeP2P (_rt, _peerInfo) {
    // GLOBALS 
    

    let _node = null; // the current node; initalized once upon start
    
    let _nodeTable = {}; // a table of the form [PeerId ↦ Connection]
    // This table is mutable: we populate it upon
    // discover of a node (either via dial or
    // handle), and remove entries upon
    // disconnects.


    let _spawnNonces = {}; // a table of the form [Nonce ↦ Callback].
    // This table is mutable: we populate it upon
    // issing a SPAWN to a remote node, and remove
    // entries from it upon receiving a successful
    // SPAWNOK mesasge that matches the nonce.

    let _whereisNonces  = {}; 

   

    async function setupConnection (conn) {
        let peerInfo = await (new Promise ( (resolve, reject) => {
            conn.getPeerInfo ((err, peerInfo) => {
                if (err) { reject (err)} else { resolve (peerInfo)}
            })
        }));

        const idKey = peerInfo.id.toB58String();
        debug(`setupConnection with ${idKey}`);
        _node.connectionManager.setPeerValue (idKey, 1);
        
        const p = Pushable()
        pull(
            p,
            pull.map(JSON.stringify),
            conn);

        _nodeTable[idKey] = p;

        pull(
            conn,
            pull.map(JSON.parse),
            pull.drain(
                (x) => { 
                    // debug (`DRAIN: ${peerInfo.isConnected()}`)
                    // peerInfo.multiaddrs.forEach( m => debug (m.toString()));

                    inputHandler(p, x, idKey) 
                },
                (err) => {
                    if (err) {
                        error (`Error in drain: ${err}`);
                    }
                    debug(`deleting entry for  ${idKey}`);
                    delete _nodeTable[idKey];
                })
        )
    }

  

    this.spawnp2p = async (id, data) => {
        let p = await getNodePushStream (id);
        const spawnNonce = uuidv4();
        return new Promise ((resolve, reject) => {
            _spawnNonces[spawnNonce] = (err, data) => {
                if (err) { reject (err) } else { resolve (data)}
            };
            p.push( {
                messageType: MessageType.SPAWN,
                spawnNonce: spawnNonce,
                message: data
            })
        });
    }

    
    this.whereisp2p = async (id, str) => {    
        let p = await getNodePushStream (id);
        let whereisNonce = uuidv4()
        
        return new Promise ( (resolve, reject) => {        
            _whereisNonces[whereisNonce] = (err, data) => {
                if (err) { reject (err) } else { resolve (data)} 
            }
            debug ("pushing whereis message")
            p.push ({
                messageType: MessageType.WHEREIS,
                whereisNonce : whereisNonce,
                message : str 
            });        
        })
    }

    this.sendp2p = async (id, procId, obj) => {
        let p = await getNodePushStream(id);
        p.push({
            messageType: MessageType.SEND,
            pid: procId,
            message: obj
        })
    }

    
    let __networkPending = [];

    function tryPending() {
        if (__networkPending.length > 0 ) {            
            let n = __networkPending.length;

            debug (`####### Connect trigger: try Pending ${n}`);

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
    
    const RETRY_TIMEOUTS = [50,100,200,400,800,1600,3200,6400,10000]
    function dial (id) {
        const peerId = PeerId.createFromB58String(id);     
        debug (`looking for peer with the id ${id}`)

        

        return new Promise ((resolve, reject) => {
            let n_retries = 0;
            let retry = (increaseRetries=true, timeoutExplicit=null) => {                
                let timeoutValue = timeoutExplicit 
                                    ? timeoutExplicit 
                                    : (n_retries < RETRY_TIMEOUTS.length 
                                        ? RETRY_TIMEOUTS[n_retries] 
                                        : RETRY_TIMEOUTS[RETRY_TIMEOUTS.length - 1]
                                      )
                debug (`using retry timeout: ${timeoutValue}`)
                setTimeout (tryFindPeer, timeoutValue);
                if (increaseRetries)  {
                    n_retries ++;
                }
            }


            function DIAL (arg) {
                
                debug (`>> Dialing to:`)

                arg.multiaddrs.forEach( m => {
                    debug (m.toString() ) 
                });

                debug (">> << ")
                

                _node.dialProtocol  (arg,  '/mlmenagerie/1.0.0', (err, conn) => {
                        if (err) {
                            retry (); 
                        } else {
                            setupConnection(conn).then(resolve)
                        }
                    })    
            }

            function tryFindPeer ()  {
                debug (`>> tryFindPeer ${nPeers()}`)
                if (_node.peerBook.has(peerId)) {
                  let _pInfo = _node.peerBook.get(peerId)
                  DIAL (_pInfo);
                  return;
                }
                
            
                debug ("Calling find Peer ")
                // _node.peerRouting.findPeer (peerId, {maxTimeout:2000}, (err, _pInfo) => {
                // _node.dial(peerId, (err, conn) => {


               _node.peerRouting.findPeer (peerId, (err, _pInfo) => {                    
                    if (!err) {
                        debug ("Find Peer succeeded");
                        DIAL(_pInfo)
                        return;
                    }
                    // error handling 
                
                    if (err.code == "ERR_LOOKUP_FAILED") {
                        debug (`networking error: only ${nPeers()} connected` )
                        // retry (false, 500);
                        addPending (tryFindPeer);
                    } else {                        
                        debug (`peer search error with ${nPeers()} peers: ${err}; ${n_retries}`)
                        // we will keep retrying if the number of connected peers is supiciously low

                        if (n_retries < 10  || nPeers () < 20 ) {
                            retry();
                            // _node.hangUp (peerId, (err) =>{
                            //     if (err) {
                            //         debug (`hangup error, ${err}`);                                    
                            //     }
                                
                            // });
                        } else {
                            debug (`too many retries, ${err}`)
                            reject (err);   
                        }
                    }

               });
               
            }
        
            tryFindPeer(); 
        });
    } 


    async function getNodePushStream (id) {    
        if (!_nodeTable[id]) {         
            await dial (id);
            
        } 
        return _nodeTable[id];
    }


    async function inputHandler(p, input, fromNodeId) {
        // info ("input handler")
        switch (input.messageType) {
            case (MessageType.SPAWN):
            if (_rt.remoteSpawnOK()) {
                // info ("RECEIVED SPAWN")
                let x = await _rt.spawnFromRemote (input.message, fromNodeId)

                p.push({
                    messageType: MessageType.SPAWNOK,
                    spawnNonce: input.spawnNonce,
                    message: x
                });
                break;
            } // drop the message otherwise

            case (MessageType.SPAWNOK):
                // info ("SPAWN OK")
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
                // debug ("received " + fromNodeId);
                _rt.receiveFromRemote (
                    input.pid,
                    input.message,
                    fromNodeId
                )
                break;

            case (MessageType.WHEREIS): 
                debug ("p2p whereis incoming request")
                let y = await _rt.whereisFromRemote (input.message)
                p.push ({
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
                break;
                // info ("received data", input.toString('utf8').replace('\n', ''));
        }
    }




    let _connections = {}


    function onPeerDiscovery (peer)  {
        const idStr = peer.id.toB58String()
        // debug (`discovery: ${idStr}`);

        
        if (_connections[idStr]) {
            // If we're already trying to connect to this peer, dont dial again
            //  debug(`_connections hit on ${idStr}`)
            return
        }
        
        // debug(`+++ discovered: ${idStr}, ${_connectCounter}`)
        
        _connections[idStr] = true

        _node.dial(peer, (err) => {  
            if (err) {        
                
                // Prevent immediate connection retries from happening
                // and include a 10s jitter

                const timeToNextDial = 25 * 1000 + (Math.random(0) * 10000).toFixed(0)
                 debug(`Failed to dial: ${idStr} ${nPeers()}`)
                setTimeout(() => delete connections[idStr], timeToNextDial)                       
            }                
        })

        // debug(`Discovered: ${peer.id.toB58String()}`)
        
    }

    function nPeers( ) {
        // return Object.keys(_node.peerBook.getAll()).length;

        return _node.connectionManager._peers.size;
    }
 
    this.startNode = async () => {    
        debug("starting p2p node")
        _node = new Node({
            peerInfo: _peerInfo
        });
        
        

        // wrapping up the cb-based call to promises
        await (new Promise ((resolve, reject) => {             
            _node.start ( (err)=> {    
                if (err){ reject (err) } else { 
                    resolve () 
                }
                } )             
        }))


        debug("p2p node started")
        
        _node.on('peer:discovery', onPeerDiscovery );
        


        _node.on('error', (err) => {
            error (`Error in p2p: ${err}`);        
        })


        // Uncomment these to see the effect of 
        // the rate limiting by connection manager
        // 2019-03-12; AA
    
        
        _node.on ('peer:connect', (peerInfo)=>{
            tryPending ();
            // let idStr = peerInfo.id.toB58String();            
            // debug (`+++ connect:    ${idStr}   ${nPeers()}` );
        })
        /*

        _node.on ('peer:disconnect', (peerInfo) => {
            let idStr = peerInfo.id.toB58String();            
            debug (`--- disconnect: ${idStr}   ${nPeers()}` );
        })
        
        */

        _node.handle("/mlmenagerie/1.0.0", (protocol, conn) => {
            setupConnection(conn)
        })


        // _node.connectionManager.on('disconnect:preemptive', (peerId) => {
        //     debug(`disconnect preeemptive ${peerId.toB58String()}  ${nPeers()}` )
        // })

        // _node.connectionManager.on ('peer:connect', (peerInfo)=>{
        //     let idStr = peerInfo.id.toB58String();            
        //     debug (`+++ connect:    ${idStr}   ${nPeers()}` );
        // })

        // _node.connectionManager.on ('peer:disconnect', (peerInfo) => {
        //     let idStr = peerInfo.id.toB58String();            
        //     debug (`--- disconnect: ${idStr}   ${nPeers()}` );
        // })

        function showNPeers ()  {
            if (logLevel == 'debug') {
                debug(`--------- n peers: ${nPeers()}`);
                setTimeout(showNPeers, 5000)            ;
            }
        }

        showNPeers();

        return _node;
    }

    this.stop = (cb) => {
        _node.stop(cb);
    } 
    
}


let _troupeP2P = null;

async function startp2p(nodeId, rt) {        
    // kick off the network initialization by loading the peerid
    let peerInfo = await loadPeerId(nodeId);
    _troupeP2P = new TroupeP2P( rt, peerInfo );
    let _n = await (_troupeP2P.startNode ());
    
    process.on('SIGINT', function() {
        console.log("Caught interrupt signal");
    
    });

    rt.networkReady(peerInfo.id.toB58String());
}




module.exports = {
    startp2p: startp2p,
    spawnp2p: (arg1, arg2) => _troupeP2P.spawnp2p(arg1, arg2),
    sendp2p: (arg1, arg2, arg3) => _troupeP2P.sendp2p(arg1, arg2, arg3),
    whereisp2p: (arg1, arg2) => _troupeP2P.whereisp2p (arg1, arg2),
    stopp2p: (cb) => {
            _troupeP2P.stop(cb)
        }
    
}
