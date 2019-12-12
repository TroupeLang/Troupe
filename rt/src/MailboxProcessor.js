'use strict'
const LVal = require('./Lval.js').LVal;
const Thread = require('./Thread.js').Thread;
const proc = require('./process.js');
const logger = require('./logger.js').mkLogger('mbox');
const debug = x => logger.debug(x)

const SandboxStatus = require('./SandboxStatus.js').HandlerState;

const levels = require('./options.js');



function Message(msg, fromNodeId, pc) {
    let tuple = [msg, fromNodeId];
    tuple.isTuple = true; // hack! 2018-10-19: AA
    return new LVal(tuple, pc);
  }



class MailboxProcessor {
    constructor(sched) {
        this.sched = sched;
        this.levels = levels;
        this.mailboxes = new Array();
    }

    setRuntimeObject (rtObj) {
        this.rtObj = rtObj;
    }

    addMessage(fromNodeId, toPid, message, pc) {
        let __sched = this.sched;
    
        // check whether the recipient is alive
        if (!__sched.isAlive(toPid)) {
            return;            
        }

        // get the recipient thread
        let t = __sched.getThread (toPid);

        // create the message 
        let messageWithSenderId = new Message(message, fromNodeId, pc);

        // add the message to the thread's mailbox
        t.addMessage (messageWithSenderId);

        // unblock the thread if necessary        
        __sched.unblockThread(toPid);
    }

    sweepMessages(messages, handlers, lowb, highb) {

        let lub = this.levels.lub;
        let flowsTo = this.levels.flowsTo;
        let __sched = this.sched;
        let __rtObj = this.rtObj;
        
        let mkBase = (f) => __sched.mkBase(f);
        let raisePC = (l) => { __sched.raiseCurrentThreadPC(l)  };
        let assertIsHandler = this.rtObj.assertIsHandler;
        let theThread = __sched.__currentThread;

        function iterate(handlerToUse, messageToCheck) {            
            debug(`* checkMessages  ${handlerToUse} ${messageToCheck} ${messages.length}`);
            if (handlerToUse < handlers.length && messageToCheck < messages.length) {
                debug("### 1");
                // debug.log (messages[messageToCheck]);

                let nextIter = (handlerToUse == handlers.length - 1) ?
                    () => {
                        iterate(0, messageToCheck + 1)
                    } :
                    () => {
                        iterate(handlerToUse + 1, messageToCheck)
                    };

                // we need two arguments because this function is later used
                // in patFail which is called from the userland, and therefore
                // must adhere to our (env, arg) compliation convention.

                let senderPC = messages[messageToCheck].lev

                let msglvl = lub(senderPC, messages[messageToCheck].val[0].lev); // 2018-05-18!AA
                if (!(flowsTo(lowb, msglvl)) || !(flowsTo(msglvl, highb))) {
                    //debug.log("* checkMessages - skipping message because of rcv bounds");
                                      
                    __sched.schedule(nextIter, [null, null], null);
                    //
                } else {                    
                    let lh = handlers[handlerToUse];
                    assertIsHandler (lh);
                    // let threadState = theThread.exportState();


                    let guard = (arg) => {
                        
                        // theThread.importState (threadState);
                        __rtObj.assertIsNTuple(arg, 2);
                        let status = arg.val[0];
                        theThread.raiseCurrentThreadPC(status.lev);
                        
                        
                        switch (status.val) {
                            case 0:                                 
                                let funclos = arg.val[1];
                                __rtObj.assertIsFunction (funclos);
                                messages.splice(messageToCheck, 1);
                                theThread.raiseBlockingThreadLev( lub (senderPC, __sched.pc))  // 2018-11-29; AA, the lub is probably redundant...                        
                                theThread.handlerState = new SandboxStatus.NORMAL();
                                __rtObj.tailcall ( funclos, __sched.__unit);                        
                                break;
                            default:
                                nextIter();
                                break;
                        }                        
                    }
                    
                    theThread.callInThread (guard);

                    theThread.handlerState = new SandboxStatus.INHANDLER ( mkBase ( (env, arg) => {
                        __rtObj.ret (theThread.mkVal (__rtObj.mkTuple ([theThread.mkVal (1), __sched.unit]) )); // trigger next iter
                    }));

                    raisePC(lh.lev);
                    let h = lh.val;
                    
                    let args = [ h.env, messages[messageToCheck] ];
                    // run the handler
                    __sched.schedule(h.fun, args, h.namespace);
                }
            } else {
                debug("### 2");
                function futureMessage() {
                    debug ("unblocking");
                    iterate(0, messageToCheck);
                }

                __sched.__currentThread.block (futureMessage) ; 
                // __sched.__currentThread.handlerState =  new HandlerState.INHANDLER(null);                
                __sched.blockThread (__sched.__currentThread);                                
            }
        }

        if (!__sched.handlerState.isNormal()) {
            
            __rtObj.threadError ("invalid handler state in receive: side effects are prohbited in restricted state");
        }
        
        iterate(0, 0);
    }



    rcv(lowb, highb, handlers) {
        let __sched = this.sched;        
        let mb = __sched.__currentThread.mailbox;
        this.sweepMessages(mb, handlers.val, lowb, highb);        
    }

    
    
}

module.exports = MailboxProcessor;
