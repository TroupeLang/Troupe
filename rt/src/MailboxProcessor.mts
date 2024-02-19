import { assertIsHandler, assertIsNTuple, assertIsFunction } from "./Asserts.mjs";
import { mkTuple } from "./ValuesUtil.mjs";
import { SchedulerInterface } from "./SchedulerInterface.mjs";
import { __unit } from "./UnitVal.mjs";
import { RuntimeInterface } from "./RuntimeInterface.mjs";

import yargs from 'yargs'
let logLevel = yargs.argv.debugmailbox ? 'debug': 'info'

import { mkLogger } from './logger.mjs'
const logger = mkLogger('MBX', logLevel);
const debug = x => logger.debug(x);
import  { HandlerState as SandboxStatus }  from  './SandboxStatus.mjs' ;
import {lub,flowsTo} from './options.mjs'
import * as levels from './options.mjs'
import { ReceiveTaintAction } from "./ReceiveTaintAction.mjs";
import { LVal, MbVal } from "./Lval.mjs";
import { MailboxInterface } from "./MailboxInterface.mjs";
import { Level } from "./Level.mjs";
import { Thread } from "./Thread.mjs";


function createMessage(msg, fromNodeId, pc) {
    let tuple:any = mkTuple ([msg, fromNodeId]);  
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
    /*

    sweepMessages(messages, handlers, lowb, highb, l_clear, taintLimit, taintAction) {


        debug (`receive interval: [${lowb.stringRep()}, ${highb.stringRep()}]`)

        let lub = this.levels.lub;
        let glb = this.levels.glb;
        let flowsTo = this.levels.flowsTo;
        let __sched = this.sched;
        let __rtObj = this.rtObj;
        let theThread = __sched.__currentThread;

        if (!theThread.handlerState.isNormal()) {            
            theThread.threadError ("invalid handler state in receive: side effects are prohbited in restricted state");
        }

        
        let raisePC = (l) => { theThread.raiseCurrentThreadPC(l)  };


        let guard_sp:number = null;

        theThread.handlerState = new SandboxStatus.INHANDLER (
            () => {
                theThread._sp = guard_sp
                return theThread.returnImmediateLValue (
                    theThread.mkVal (mkTuple ([theThread.mkVal (1), __unit]) ))
            }    
            // Errors in the evaluation of the handler will trigger next iteration.
            // See iterate function below
            // 
        , theThread.pc
        );

        function iterate(handlerToUse, messageToCheck) {            
            debug(`* checkMessages  ${handlerToUse} ${messageToCheck} ${messages.length}`);
            if (handlerToUse < handlers.length && messageToCheck < messages.length) {
                debug("### 1");
                debug (`iterate ${messages[messageToCheck].stringRep()}  || l_clear = ${l_clear.stringRep()}`);

                let nextIter = (handlerToUse == handlers.length - 1) ?
                    () => { return iterate(0, messageToCheck + 1) } :
                    () => { return iterate(handlerToUse + 1, messageToCheck) };

                // we need two arguments because this function is later used
                // in patFail which is called from the userland, and therefore
                // must adhere to our (env, arg) compliation convention.


                // 2020-02-08; observe that the value that gets bound to senderPC may be further 
                // than the original sender's pc, because it is earlier raised by up to the 
                // clearance level
                let senderPC = messages[messageToCheck].lev
                debug (`receive/sender pc is ${senderPC.stringRep()}`)

                // let msglvl = lub(senderPC, messages[messageToCheck].val[0].lev); // 2018-05-18!AA
                let msglvl = senderPC 
                if (!(flowsTo(lowb, msglvl)) || !(flowsTo(msglvl, highb))) {
                    debug("* checkMessages - skipping message because of rcv bounds");
                    return nextIter 
                    // __sched.schedule(nextIter, [null, null], null);
                    //
                } else {                    
                    debug ("executing handler")
                    let lh = handlers[handlerToUse];
                    assertIsHandler (lh);
                    // let threadState = theThread.exportState();

                    let _pcBeforeHandler = theThread.pc
                    let _blBeforeHandler = theThread.bl 

                    debug (`_pcBeforeHandler = ${_pcBeforeHandler.stringRep()} | _blBeforeHandler = ${_blBeforeHandler.stringRep()}`)
                    let guardReturnPoint:any = () => {      
                        // theThread.importState (threadState);
                        let arg = theThread.arg_as_lval 
                        debug (`arg in pushed frame ${arg.stringRep()}`)
                        assertIsNTuple(arg, 2);
                        let status = arg.val[0];
                        theThread.raiseCurrentThreadPC(lub ( status.lev, arg.lev) );
                        if (flowsTo (theThread.bl, taintLimit )) {
                            switch (status.val) {
                                case 0:                                 
                                    let funclos = arg.val[1];
                                    assertIsFunction (funclos);
                                    messages.splice(messageToCheck, 1);
                                    theThread.raiseBlockingThreadLev( lub (senderPC, __sched.__currentThread.pc))  // 2018-11-29; AA, the lub is probably redundant...                        
                                    theThread.invalidateSparseBit() // 2021-05-12; AA: TODO: consider optimizing this and actually using the received value
                                    theThread.handlerState = new SandboxStatus.NORMAL();                                                                        
                                    debug ("setting the thread state back to normal")
                                    return theThread.tailCall ( funclos.val, __unit);
                                default:
                                    return nextIter;                                    
                            }   
                        } else {
                            let idx;
                            switch (taintAction) {
                                case ReceiveTaintAction.KEEP:
                                    idx = messageToCheck + 1
                                    break; 
                                case ReceiveTaintAction.DROP:
                                    idx = messageToCheck
                                    messages.splice(messageToCheck, 1);
                            }                            
                            // restore the pc,bl levels (but bound by the taintLimit)  
                                                  
                            theThread.pc = lub (_pcBeforeHandler, taintLimit)
                            theThread.bl = lub (_blBeforeHandler, taintLimit)
                            
                            // onto the next message
                            return iterate(0, idx)
                        }                        
                    }
                    guardReturnPoint.debugname = "<guardReturnPoint>"
                    theThread.pushFrame ( guardReturnPoint )
                    guard_sp = theThread._sp // see the code for the INHANDLER above that uses this to unwind the stack
                   
                    raisePC(lh.lev);
                    let h = lh.val;
                    let msg_orig = messages[messageToCheck]

                    
                    // 
                    // 2020-02-08: AA; this is the place where we are raising the level 
                    // of the message from the mailbox to that of the lclear
                    // 
                    //  debug (`about to tail into the handler code\n  ${h.toString()}`)
                    let msg_raised_to_lclear = new LVal ( msg_orig.val, lub (msg_orig.lev, highb, l_clear))
                    return theThread.tailCall (h, msg_raised_to_lclear);
                    
                }
            } else {
                debug("### 2");
                function futureMessage() {
                    // debug ("unblocking");
                    return iterate(0, messageToCheck);
                }

                theThread.block (futureMessage) ; 
                // __sched.__currentThread.handlerState =  new HandlerState.INHANDLER(null);                
                __sched.blockThread (theThread);   
            }
        }

        
        
        return iterate(0, 0);
    }


    rcv(lowb, highb, handlers, l_clear, taintLimit = levels.TOP, taintAction = ReceiveTaintAction.KEEP) {
      let __sched = this.sched;        
      let mb = __sched.__currentThread.mailbox;
      return this.sweepMessages(mb, handlers.val.toArray(), lowb, highb, l_clear,taintLimit, taintAction);
      
    } 
    
    */

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
        debug (`consume interval: [${lowb.stringRep()}, ${highb.stringRep()}]`)
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

