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
      // TODO check "What is next?" section
      // announce: ['/dns4/auto-relay.libp2p.io/tcp/443/wss/p2p/QmWDn2LY8nannvSWJzruUYoLZ4vV83vfCBwd8DipvdgQc3']
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
    /*services: {
      identify: identifyService(),
      relay: circuitRelayServer()
    }*/
    relay: {
      enabled: true,
      hop: {
        enabled: true
      },
      advertise: {
        enabled: true,
      }
    }
  })

  await node.handle("/trouperelay/keepalive", async ({ connection, stream }) => {
    console.log(`Relay handling protocol, id: ${connection.remotePeer}`)
    //setupConnection(connection.remotePeer, stream)
  })

  console.log(`Node started with id ${node.peerId.toString()}`)
  console.log('Listening on:')
  node.getMultiaddrs().forEach((ma) => console.log(ma.toString()))
}

main()