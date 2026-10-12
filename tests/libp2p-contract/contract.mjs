// The internals of libp2p and @libp2p/circuit-relay-v2 that the Troupe runtime relies on
// (rt/src/ffi/node/p2prelaycontrol.mts and the client side of relay reservations), asserted
// by running them: a relay node R (WebSocket on 127.0.0.1:7650) and clients in this process.
// Each failure names the internal; run by `make test/libp2p-contract`, part of `make ci`.
import { createLibp2p } from 'libp2p'
import { webSockets } from '@libp2p/websockets'
import { noise } from '@chainsafe/libp2p-noise'
import { yamux } from '@chainsafe/libp2p-yamux'
import { identify } from '@libp2p/identify'
import { circuitRelayServer, circuitRelayTransport } from '@libp2p/circuit-relay-v2'
import { FaultTolerance } from '@libp2p/interface'
import { generateKeyPair } from '@libp2p/crypto/keys'
import { peerIdFromPrivateKey } from '@libp2p/peer-id'
import { multiaddr } from '@multiformats/multiaddr'

const RELAY_PORT = 7650
const HOOK_DELAY = 300
const sleep = (ms) => new Promise((res) => setTimeout(res, ms))
let passed = 0
setTimeout(() => { console.error('libp2p contract: not finished after 30 s'); process.exit(1) }, 30000).unref()

function check(internal, ok, detail) {
  if (!ok) throw new Error(`libp2p contract: ${internal}: ${detail}`)
  passed++
  console.log(`ok ${passed} ${internal}`)
}

async function failure(promise) {
  const t0 = Date.now()
  try {
    await promise
    return { ms: Date.now() - t0, text: null }
  } catch (e) {
    return { ms: Date.now() - t0, text: `${e.name}: ${e.message}` }
  }
}

async function waitFor(cond, ms) {
  for (const t0 = Date.now(); Date.now() - t0 < ms; await sleep(20)) if (cond()) return true
  return cond()
}

const hooks = { reserve: async () => false, connect: async () => false }
const R = await createLibp2p({
  addresses: { listen: [`/ip4/127.0.0.1/tcp/${RELAY_PORT}/ws`] },
  transports: [webSockets()],
  connectionEncrypters: [noise()],
  streamMuxers: [yamux()],
  connectionGater: {
    denyInboundRelayReservation: (peer) => hooks.reserve(peer),
    denyOutboundRelayedConnection: (src, dst) => hooks.connect(src, dst)
  },
  services: { identify: identify(), relay: circuitRelayServer() }
})
const relayAddr = R.getMultiaddrs()[0].toString()
const circuit = multiaddr(`${relayAddr}/p2p-circuit`)
const via = (peer) => multiaddr(`${relayAddr}/p2p-circuit/p2p/${peer}`)
const later = (ms, answer) => async () => { await sleep(ms); return answer }

const store = R.services.relay.reservationStore
check('CircuitRelayServer.reservationStore (server/index.js)', store?.reservations === R.services.relay.reservations,
  'the relay service has no field reservationStore holding its reservations map')

const clients = []
async function client(faultTolerance = FaultTolerance.FATAL_ALL) {
  const n = await createLibp2p({
    transports: [webSockets(), circuitRelayTransport()],
    connectionEncrypters: [noise()],
    streamMuxers: [yamux()],
    transportManager: { faultTolerance },
    services: { identify: identify() }
  })
  clients.push(n)
  return n
}
const reserve = (n) => n.components.transportManager.listen([circuit])

// The relay service awaits the reservation hook; a deny reaches the client as PERMISSION_DENIED.
hooks.reserve = later(HOOK_DELAY, true)
const X = await client()
let f = await failure(reserve(X))
check('relay service awaits connectionGater.denyInboundRelayReservation', f.text !== null && f.ms >= HOOK_DELAY,
  `listen settled after ${f.ms} ms (${f.text ?? 'resolved'}); expected a deny after ${HOOK_DELAY} ms`)
check('a denied reservation rejects listen with PERMISSION_DENIED (FATAL_ALL)', /PERMISSION_DENIED/.test(f.text),
  `the error was: ${f.text}`)
const Y = await client(FaultTolerance.NO_FATAL)
f = await failure(reserve(Y))
check('a denied reservation resolves listen without a reservation (NO_FATAL)',
  f.text === null && !R.services.relay.reservations.has(Y.peerId), `listen gave ${f.text ?? 'resolved'}`)

// listen on <relay>/p2p-circuit reserves; the transport's reservationStore emits the events.
hooks.reserve = async () => false
const B = await client()
const transport = B.components.transportManager.getTransports()
  .find((t) => t[Symbol.toStringTag] === '@libp2p/circuit-relay-v2-transport')
const events = []
for (const name of ['relay:created-reservation', 'relay:removed']) {
  transport?.reservationStore?.addEventListener(name, (e) => events.push([name, e.detail.relay.toString()]))
}
check('CircuitRelayTransport.reservationStore (transport/index.js)',
  typeof transport?.reservationStore?.addEventListener === 'function', 'no event target in field reservationStore')
f = await failure(reserve(B))
check('transportManager.listen on <relay>/p2p-circuit creates a reservation',
  f.text === null && R.services.relay.reservations.has(B.peerId), `listen gave ${f.text ?? 'resolved, no reservation'}`)
check('reservationStore emits relay:created-reservation',
  events.some(([n, p]) => n === 'relay:created-reservation' && p === R.peerId.toString()), JSON.stringify(events))

// The relay service awaits the connection hook; a deny reaches the dialler as PERMISSION_DENIED.
const A = await client()
hooks.connect = later(HOOK_DELAY, true)
f = await failure(A.dial(via(B.peerId)))
check('relay service awaits connectionGater.denyOutboundRelayedConnection', f.text !== null && f.ms >= HOOK_DELAY,
  `dial settled after ${f.ms} ms (${f.text ?? 'resolved'}); expected a deny after ${HOOK_DELAY} ms`)
check('a denied connection rejects the dial with PERMISSION_DENIED', /PERMISSION_DENIED/.test(f.text), `the error was: ${f.text}`)
hooks.connect = async () => false
const stranger = peerIdFromPrivateKey(await generateKeyPair('Ed25519'))
f = await failure(A.dial(via(stranger)))
check('a dial to an unreserved peer rejects with NO_RESERVATION', /NO_RESERVATION/.test(f.text), `the error was: ${f.text}`)

// ReservationStore.removeReservation plus a reset of the reservation's signal ends a relayed stream.
await B.handle('/contract/hold/1.0.0', () => {}, { runOnLimitedConnection: true })
const conn = await A.dial(via(B.peerId))
const stream = await conn.newStream('/contract/hold/1.0.0', { runOnLimitedConnection: true })
check('a relayed stream opens', stream.status === 'open', `stream status ${stream.status}`)
const held = store.get(B.peerId)
store.removeReservation(B.peerId)
check('ReservationStore.removeReservation deletes the entry', !R.services.relay.reservations.has(B.peerId), 'entry still present')
check('RelayReservation.signal.reset (retimeable-signal)', typeof held?.signal?.reset === 'function', 'no signal.reset')
held.signal.reset(0)
check('signal.reset(0) ends the relayed stream and connection',
  await waitFor(() => stream.status !== 'open' && conn.status !== 'open', 2000),
  `after 2 s: stream ${stream.status}, connection ${conn.status}`)

// The relay closing the connection that holds the reservation makes the client emit relay:removed.
await R.hangUp(B.peerId)
check('reservationStore emits relay:removed when the relay connection closes',
  await waitFor(() => events.some(([n]) => n === 'relay:removed'), 2000), JSON.stringify(events))

// The four limit fields of the running relay service's store govern the next reservation.
for (const field of ['reservationTtl', 'maxReservations', 'defaultDurationLimit', 'defaultDataLimit']) {
  check(`ReservationStore.${field} (server/reservation-store.js)`, field in store, 'no such field')
}
Object.assign(store, { reservationTtl: 2000, defaultDurationLimit: 1500, defaultDataLimit: 8192n })
const C = await client()
await reserve(C)
const r = R.services.relay.reservations.get(C.peerId)
const ttl = r == null ? NaN : r.expiry.getTime() - Date.now()
check('ReservationStore.reservationTtl sets the expiry of the next reservation', ttl > 1000 && ttl <= 2000, `expiry in ${ttl} ms`)
check('ReservationStore.defaultDurationLimit and defaultDataLimit set its limit',
  r?.limit?.duration === 1500 && r?.limit?.data === 8192n, `limit ${r?.limit?.duration} ms, ${r?.limit?.data} bytes`)
check('the reservation ends at its expiry', await waitFor(() => !R.services.relay.reservations.has(C.peerId), 3000),
  'still present 3 s after a 2 s lifetime')
store.maxReservations = R.services.relay.reservations.size
f = await failure(reserve(await client()))
check('ReservationStore.maxReservations refuses the next reservation', /RESERVATION_REFUSED/.test(f.text), `listen gave ${f.text ?? 'resolved'}`)

await Promise.all(clients.map((n) => n.stop()))
await R.stop()
console.log(`libp2p contract: ${passed} checks passed`)
process.exit(0)
