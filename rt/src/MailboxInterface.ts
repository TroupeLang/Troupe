import { Level } from "./Level";
import { ReceiveTaintAction } from "./ReceiveTaintAction";

export interface MailboxInterface {
    // rcv(pc: any, pc2: any, handlers: any, boost_level: any, taintLimit?: Level, taintAction?: ReceiveTaintAction );
    peek(lev: Level, i: number, lowb: Level, highb: Level)
    consume (lev: Level, i: number, lowb: Level, highb: Level)
}