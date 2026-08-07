import { assertIsHandler, assertIsNTuple, assertIsFunction } from "./Asserts.mjs";
import { mkTuple } from "./ValuesUtil.mjs";
import { SchedulerInterface } from "./SchedulerInterface.mjs";
import { __unit } from "./UnitVal.mjs";
import { RuntimeInterface } from "./RuntimeInterface.mjs";

import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
const argv = getCliArgs();
let logLevel = argv[TroupeCliArg.DebugMailbox] ? 'debug': 'info'

import { mkLogger, mkDebugTag } from './logger.mjs'
const logger = mkLogger('MBX', logLevel);
// Tagged template: `debug `...${v}...`` builds the string (and stringReps
// its values) only when debug logging is on — these run per message.
const debug = mkDebugTag(logger);

// Quarantine-specific logger
const qrnLogLevel = argv[TroupeCliArg.DebugQuarantine] ? 'debug' : 'info';
const qrnLogger = mkLogger('QRN', qrnLogLevel);
const qdebug = mkDebugTag(qrnLogger);
import  { HandlerState as SandboxStatus }  from  './SandboxStatus.mjs' ;
import {lub,flowsTo} from './Level.mjs'
import * as levels from './Level.mjs'
import { ReceiveTaintAction } from "./ReceiveTaintAction.mjs";
import { LVal, MbVal } from "./Lval.mjs";
import { MailboxInterface } from "./MailboxInterface.mjs";
import { Level } from "./Level.mjs";
import { Thread } from "./Thread.mjs";
import { Record } from "./Record.mjs";
import { wrapQuarantineAuth } from "./QuarantineUtils.mjs";


function createMessage(msg, fromNodeId, pc, quarantineAuthLVal: LVal | null = null) {
    // Create metadata record with senderNode field
    // This enables the handler syntax: hn pattern | {senderNode=node} => ...
    let metadataFields: [string, any][] = [["senderNode", fromNodeId]];
    if (quarantineAuthLVal !== null) {
        // Add quarantine authority to metadata (already an LVal containing Authority)
        metadataFields.push(["quarantineAuth", quarantineAuthLVal]);
    }
    let metadata = Record.mkRecord(metadataFields);
    let tuple: any = mkTuple([msg, new LVal(metadata, fromNodeId.lev)]);
    return new MbVal(tuple, pc);
}


export class MailboxProcessor implements MailboxInterface {
    sched: SchedulerInterface;
    levels: any; 
    mailboxes : any [];
    rtObj: RuntimeInterface


    
    constructor(rtObj:RuntimeInterface) {
        this.levels = levels;
        this.mailboxes = new Array();
        this.rtObj = rtObj        
        this.sched = rtObj.__sched
    }



    addMessage(fromNode: string, toPid, message, pc, quarantineAuth: Level | null = null) {

        debug `addMessage ${message} ${pc}`
        let __sched = this.sched;

        // check whether the recipient is alive
        if (!__sched.isAlive(toPid)) {
            return;
        }

        // get the recipient thread
        let t = __sched.getThread (toPid);

        // Construct fromNodeId using the receiving thread's creation-time PC
        // This ensures the label reflects when the thread was created, not when
        // the message was received
        let metadataLev = lub(pc, t.pcAtCreation());
        let fromNodeId = new LVal(fromNode, metadataLev);

        // Create quarantine authority LVal if present
        let quarantineAuthLVal = wrapQuarantineAuth(quarantineAuth, metadataLev);

        if (quarantineAuthLVal !== null) {
            qdebug `MAILBOX: delivering quarantined message to pid=${toPid.val.pid} auth=${quarantineAuth}`;
        }

        // create the message with optional quarantine authority
        let messageWithSenderId = createMessage(message, fromNodeId, pc, quarantineAuthLVal);

        // add the message to the thread's mailbox
        t.addMessage (messageWithSenderId);

        // unblock the thread if necessary
        __sched.unblockThread(toPid);
    }

    findFrom (theThread: Thread, i : number, j:number, index:number, lowb: Level, highb: Level, kont ) {
        let mb = theThread.mailbox;
        let _peekF = (i : number, j: number) => {
            if (i < mb.head) { i = mb.head }
            for (; i < mb.length; i ++ ) {
                let msg_i = mb[i]
                debug `mailbox iteration ${i} ${j} ${msg_i}`
                let presenceLev = msg_i.lev
                debug `presence level is ${presenceLev}`
                if (!(flowsTo(lowb, presenceLev)) || !(flowsTo(presenceLev, highb))) {
                    debug `* skipping message because it is outside of the interval bounds`
                    continue;
                } else {
                    debug `* message is within the interval ${j} ${index}`
                    if ( j == index ) {
                        debug `* find match; returning`
                        return kont (i)
                    } else {
                        j++
                    }
                }
            }
            debug `* blocking `
            theThread.block (() => {
                    debug ` * unblocking *`
                    return _peekF (i,j)
                })
                
            this.sched.blockThread(theThread)
        }        

        return _peekF (i,j)
    }


    peek(lev: Level, index: number, lowb: Level, highb: Level) {        
        let theThread = this.sched.__currentThread
        let mb = theThread.mailbox;
        debug `peek index: ${index}`
        debug `peek interval: [${lowb}, ${highb}]`
        theThread.raiseBlockingThreadLev (highb)
        theThread.invalidateSparseBit()
        let _i = mb.head, _j = 0

        if (mb.peek_cache_index < index && mb.peek_cache_lowb == lowb
                                        && mb.peek_cache_highb == highb) {
                debug `* peek cache hit`
            _i = mb.peek_cache_position + 1
            _j = mb.peek_cache_index + 1 
        }

        return this.findFrom ( theThread , _i , _j , index , lowb , highb
                            , (i:number) => {
                                    mb.peek_cache_index = index 
                                    mb.peek_cache_position = i
                                    mb.peek_cache_lowb = lowb
                                    mb.peek_cache_highb = highb 
                                    let newLev =        lub (mb[i].lev, lev)
                                    debug `* peek returns value at level ${newLev}`
                                    return theThread.returnImmediateLValue (
                                        new LVal (mb[i].val,
                                                    newLev,
                                                    newLev 
                                                  ))
                            })
    }

    consume(lev: Level, index: number, lowb: Level, highb: Level) {
        let theThread = this.sched.__currentThread
        let mb = theThread.mailbox;
        debug `consume index: ${index}`
        debug `consume interval: [${lowb} to ${highb}]`
        theThread.raiseBlockingThreadLev (highb)
        theThread.invalidateSparseBit()
        let kontFound = (i:number) => {
            mb.resetPeekCache ();
            let foundValue = mb[i]
            mb.consumeAt (i)
            return theThread.returnImmediateLValue (
                new LVal (foundValue.val, lub (foundValue.lev, lev)))
        }

        if (mb.peek_cache_index == index && mb.peek_cache_lowb == lowb
                                         && mb.peek_cache_highb == highb) {
            debug `* consume exact cache hit`
            return kontFound (mb.peek_cache_position)
        }

        let _i = mb.head, _j = 0

        if (mb.peek_cache_index < index && mb.peek_cache_lowb == lowb
            && mb.peek_cache_highb == highb) {
                debug `* consume next cache hit`
                _i = mb.peek_cache_position + 1
                _j = mb.peek_cache_index + 1 
        }

        return this.findFrom ( theThread , _i , _j , index , lowb , highb, kontFound)

    }
    
}

