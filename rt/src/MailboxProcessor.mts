import { assertIsHandler, assertIsNTuple, assertIsFunction } from "./Asserts.mjs";
import { mkTuple } from "./ValuesUtil.mjs";
import { SchedulerInterface } from "./SchedulerInterface.mjs";
import { __unit } from "./UnitVal.mjs";
import { RuntimeInterface } from "./RuntimeInterface.mjs";

import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
const argv = getCliArgs();
let logLevel = argv[TroupeCliArg.DebugMailbox] ? 'debug': 'info'

import { mkLogger } from './logger.mjs'
const logger = mkLogger('MBX', logLevel);
const debug = x => logger.debug(x);
import  { HandlerState as SandboxStatus }  from  './SandboxStatus.mjs' ;
import {lub,flowsTo} from './Level.mjs'
import * as levels from './Level.mjs'
import { ReceiveTaintAction } from "./ReceiveTaintAction.mjs";
import { LVal, MbVal } from "./Lval.mjs";
import { MailboxInterface } from "./MailboxInterface.mjs";
import { Level } from "./Level.mjs";
import { Thread } from "./Thread.mjs";


function createMessage(msg, fromNodeId, pc) {
    let tuple:any = mkTuple ([msg, fromNodeId], false);  
    // tuple.isTuple = true; // hack! 2018-10-19: AA
    // tuple._troupeType = TroupeType.TUPLE
    // tuple.dataLevel = lub (msg.dataLevel, pc)
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



    addMessage(fromNodeId, toPid, message, pc) {        

        debug (`addMessage ${message.stringRep()} ${pc.stringRep()}`)
        let __sched = this.sched;
    
        // check whether the recipient is alive
        if (!__sched.isAlive(toPid)) {
            return;            
        }

        // get the recipient thread
        let t = __sched.getThread (toPid);

        // create the message 
        let messageWithSenderId = createMessage(message, fromNodeId, pc);

        // add the message to the thread's mailbox
        t.addMessage (messageWithSenderId);

        // unblock the thread if necessary        
        __sched.unblockThread(toPid);
    }

    findFrom (theThread: Thread, i : number, j:number, index:number, lowb: Level, highb: Level, kont ) {
        let mb = theThread.mailbox;
        let _peekF = (i : number, j: number) => {
            for (; i < mb.length; i ++ ) {
                let msg_i = mb[i]
                debug (`mailbox iteration ${i} ${j} ${msg_i.stringRep()}`)
                let presenceLev = msg_i.lev 
                debug (`presence level is ${presenceLev.stringRep()}`)
                if (!(flowsTo(lowb, presenceLev)) || !(flowsTo(presenceLev, highb))) {
                    debug("* skipping message because it is outside of the interval bounds");
                    continue; 
                } else {        
                    debug (`* message is within the interval ${j} ${index}`) 
                    if ( j == index ) {     
                        debug (`* find match; returning`)
                        return kont (i)
                    } else {
                        j++
                    }
                }
            }                
            debug (`* blocking `)
            theThread.block (() => {
                    debug (` * unblocking *`)
                    return _peekF (i,j)
                })
                
            this.sched.blockThread(theThread)
        }        

        return _peekF (i,j)
    }


    peek(lev: Level, index: number, lowb: Level, highb: Level) {        
        let theThread = this.sched.__currentThread
        let mb = theThread.mailbox;
        debug (`peek index: ${index}`)        
        debug (`peek interval: [${lowb.stringRep()}, ${highb.stringRep()}]`)
        let lclear = mb.mclear 
        theThread.raiseBlockingThreadLev (lub (highb, lclear.boost_level))
        theThread.invalidateSparseBit()
        let _i = 0, _j = 0

        if (mb.peek_cache_index < index && mb.peek_cache_lowb == lowb 
                                        && mb.peek_cache_highb == highb) {
                debug (`* peek cache hit`)
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
                                    debug (`* peek returns value at level ${newLev.stringRep()}`)
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
        debug (`consume index: ${index}`)        
        debug (`consume interval: [${lowb.stringRep()} to ${highb.stringRep()}]`)
        let lclear = mb.mclear 
        theThread.raiseBlockingThreadLev (lub (highb, lclear.boost_level))
        theThread.invalidateSparseBit()
        let kontFound = (i:number) => {
            mb.resetPeekCache ();
            let foundValue = mb[i]
            mb.splice (i, 1)
            return theThread.returnImmediateLValue (
                new LVal (foundValue.val, lub (foundValue.lev, lev)))
        }

        if (mb.peek_cache_index == index && mb.peek_cache_lowb == lowb 
                                         && mb.peek_cache_highb == highb) {
            debug (`* consume exact cache hit`)
            return kontFound (mb.peek_cache_position)
        }

        let _i = 0, _j = 0

        if (mb.peek_cache_index < index && mb.peek_cache_lowb == lowb 
            && mb.peek_cache_highb == highb) {
                debug (`* consume next cache hit`)
                _i = mb.peek_cache_position + 1
                _j = mb.peek_cache_index + 1 
        }

        return this.findFrom ( theThread , _i , _j , index , lowb , highb, kontFound)

    }
    
}

