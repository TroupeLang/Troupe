import { Level } from "./Level.mjs";
import { ReceiveTaintAction } from "./ReceiveTaintAction.mjs";

export interface MailboxInterface {
    // rcv(pc: any, pc2: any, handlers: any, boost_level: any, taintLimit?: Level, taintAction?: ReceiveTaintAction );
    peek(lev: Level, i: number, lowb: Level, highb: Level)
    consume (lev: Level, i: number, lowb: Level, highb: Level)
    // The ingress path: used by the network (receiveFromRemote) and by the
    // terminal event handlers, both of which run from Node callbacks with no
    // current thread and pass the presence level explicitly.
    addMessage (fromNode: string, toPid: any, message: any, pc: Level,
                quarantineAuth?: Level | null)
}