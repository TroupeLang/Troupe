'use strict'

const Libp2p = require('libp2p')
const Id = require('peer-id')
const Info = require('peer-info')
const multiaddr = require('multiaddr')
const pull = require('pull-stream')

const Stardust = require('libp2p-stardust')

Id.create((err, id) => { // generate a random id for testing
  if (err) { throw err } // re-throw any error that might have occured

  const peerInfo = new Info(id)
  peerInfo.multiaddrs.add(multiaddr('/dns4/stardust.mkg20001.io/tcp/443/wss/p2p-stardust/'))

  const stardust = new Stardust({ id }) // the id is required to prove the client's identity to the server

  const modules = {
    transport: [
      stardust
    ],
    discovery: [
      stardust.discovery
    ]
  }

  const node = new Libp2p(modules, peerInfo) // create a libp2p node with the stardust transport

  node.handle('/test/1.0.0', (protocol, conn) => {
    pull(
      pull.values(['hello']),
      conn,
      pull.map((s) => s.toString()),
      pull.log()
    )
  })

  node.start((err) => {
    if (err) {
      throw err
    }

    node.dial(peerInfo, '/test/1.0.0', (err, conn) => {
      if (err) {
        throw err
      }

      pull(
        pull.values(['hello from the other side']),
        conn,
        pull.map((s) => s.toString()),
        pull.log()
      )
    })
  })
})