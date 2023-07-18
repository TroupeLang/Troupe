import { noise } from '@chainsafe/libp2p-noise'
import { yamux } from '@chainsafe/libp2p-yamux'
import { mplex } from '@libp2p/mplex'
import { webSockets } from '@libp2p/websockets'
import { createLibp2p } from 'libp2p'
import { circuitRelayServer } from 'libp2p/circuit-relay'
import { identifyService } from 'libp2p/identify'
import { createFromJSON } from '@libp2p/peer-id-factory'

async function main () {
  const id = await createFromJSON({id : "12D3KooWShh9qmeS1UEgwWpjAsrjsigu8UGh8DRKyx1UG6HeHzjf",
                                   privKey : "CAESQEQ7HBed1HEMpRHdhDmsJOlzHsVNBEWVc7DjEzuQtElv+uET7jQtZlGNKpltf2w4P7UqMdSYm4cYAGzjHcGcSj4="});
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
        reservations: {
          defaultDurationLimit: 2147483647, // the default maximum amount of time a relayed connection can be open for
          defaultDataLimit: BigInt(4294967295), // the default maximum number of bytes that can be transferred over a relayed connection
        }
      }), //AB: find out which settings to use to not cut off connections
    }
  })

  await node.handle("/trouperelay/keepalive", async ({ connection, stream }) => {
    console.log(`Relay handling protocol, id: ${connection.remotePeer}`)
  })

  console.log(`Node started with id ${node.peerId.toString()}`)
  console.log('Listening on:')
  node.getMultiaddrs().forEach((ma) => console.log(ma.toString()))
}

main()