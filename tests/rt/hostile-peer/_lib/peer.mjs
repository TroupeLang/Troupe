// Shared primitives for hostile-peer tests: a raw libp2p peer that speaks the
// Troupe wire protocol well enough to open a stream, but is free to send bytes
// no cooperating Troupe node ever would. Mirrors the node's libp2p config
// (libp2p 3.x, tcp, noise, yamux) so the handshake completes.
import { createLibp2p } from 'libp2p'
import { tcp } from '@libp2p/tcp'
import { noise } from '@chainsafe/libp2p-noise'
import { yamux } from '@chainsafe/libp2p-yamux'
import { multiaddr } from '@multiformats/multiaddr'
import * as lp from 'it-length-prefixed'
import { pipe } from 'it-pipe'
import { fromString as u8FromString } from 'uint8arrays/from-string'

export const TROUPE_PROTOCOL = '/troupe/1.0.0'

// Dial a victim multiaddr and open the Troupe protocol stream.
export async function connect(targetAddr) {
  const node = await createLibp2p({
    transports: [tcp()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
  })
  await node.start()
  const stream = await node.dialProtocol(
    multiaddr(targetAddr), TROUPE_PROTOCOL, { runOnLimitedConnection: true })
  return { node, stream }
}

// Send one length-prefixed frame. `payload` may be a string or Uint8Array; the
// framing matches it-length-prefixed on the receiving side, so the bytes reach
// the read pipe as a single message regardless of their content.
export async function sendFrame(stream, payload) {
  const bytes = payload instanceof Uint8Array ? payload : u8FromString(String(payload), 'utf8')
  await pipe(
    [bytes],
    (source) => lp.encode(source),
    async (source) => { for await (const chunk of source) { stream.send(chunk) } }
  )
}

// Give the victim a moment to process, then tear down the attacker cleanly.
export async function close(node, graceMs = 1000) {
  await new Promise(r => setTimeout(r, graceMs))
  await node.stop()
}
