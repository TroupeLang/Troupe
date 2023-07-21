import { noise } from '@chainsafe/libp2p-noise';
import { yamux } from '@chainsafe/libp2p-yamux';
import { mplex } from '@libp2p/mplex';
import { webSockets } from '@libp2p/websockets';
import { createLibp2p } from 'libp2p';
import { circuitRelayServer } from 'libp2p/circuit-relay';
import { identifyService } from 'libp2p/identify';
import { createFromJSON } from '@libp2p/peer-id-factory';
import { pipe } from 'it-pipe';
import * as lp from 'it-length-prefixed';
import map from 'it-map';
import { toString as uint8ArrayToString } from 'uint8arrays/to-string';
import { readFileSync } from 'fs';

async function main () {
  const relayId = readFileSync("keys/relay.id");
  const relayKey = readFileSync("keys/relay.priv");
  const id = await createFromJSON({id : relayId, privKey : relayKey});

  const node = await createLibp2p({
    peerId : id,
    addresses: {
      listen: ['/ip4/0.0.0.0/tcp/5555/ws']
    },
    transports: [
      webSockets()
    ],
    connectionEncryption: [
      noise()
    ],
    streamMuxers: [
      yamux(),
      mplex()
    ],
    services: {
      identify: identifyService(),
      relay: circuitRelayServer({ // makes the node function as a relay server
        /*
          The limits are set intentionally very high to avoid the relay cutting off
          the connection. This can seemingly not be disabled in any other way.
          (See: https://github.com/libp2p/specs/blob/f5c5829ef9753ef8b8a15d36725c59f0e9af897e/relay/circuit-v2.md?plain=1#L71)
          There is also no way to distinguish whether the relay cut off the connection
          because of a time/data limit or the other party cut off the connection. Therefore,
          it is impossible to know whether to re-establish the connection or not.
          
          Two alternatives to giving large limits were considered.
          - The good solution
          Implement the connections being intentionally broken by the peers involved
          before the relay reaches either limit. This would entail both timing and
          counting all the bytes sent on each relayed connection. This was rejected
          for being too time consuming, since the relay code might change soon anyway.

          - The hacky solution
          Use a version of libp2p where the code for breaking a connection is removed.
          This was rejected to allow for easier upgrading of the libp2p library.
        */
        reservations: {
          defaultDurationLimit: 2147483647,
          defaultDataLimit: BigInt(4294967295),
        }
      }),
    }
  });

  await node.handle("/trouperelay/keepalive", async ({ connection, stream }) => {
    let id = connection.remotePeer;
    console.log(`Relay handling protocol, id: ${id}`);
    streamToConsole(stream, id);
  })

  console.log(`Relay node started with id ${node.peerId.toString()}`);
  console.log('Listening on:');
  node.getMultiaddrs().forEach((ma) => console.log(ma.toString()));
}

function streamToConsole (stream, id) {
  console.log(`Handling keep-alives from ${id}`);
  pipe(
    stream.source,
    (source) => lp.decode(source),
    (source) => map(source, (buf) => uint8ArrayToString(buf.subarray())),
    async function (source) {
      for await (const msg of source) {
        console.log(`Keep alive message from ${id}: ${msg.toString()}`);
      }
    }
  );
}

main()