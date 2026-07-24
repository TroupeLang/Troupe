// Sends the victim a single length-prefixed frame whose payload is not valid
// JSON. On the victim this reaches JSON.parse in the read pipe before any trust
// check or deserialization. A node without inbound-frame handling terminates
// here; a hardened node drops the connection and keeps serving.
import { connect, sendFrame, close } from '../_lib/peer.mjs'

const target = process.argv[2]
if (!target) { console.error('usage: attack.mjs <victim-multiaddr>'); process.exit(2) }

const { node, stream } = await connect(target)
console.log('ATTACK: sending one malformed (non-JSON) frame')
await sendFrame(stream, 'this is not valid json }{')
await close(node)
process.exit(0)
