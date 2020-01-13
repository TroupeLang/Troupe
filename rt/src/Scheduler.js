'use strict';
const uuidv4 = require('uuid/v4');
const process = require('./process.js');

const BaseFunction = require('./BaseFunction.js').BaseFunction;
const BaseKont = require('./BaseKont.js')
const LVal = require('./Lval.js').LVal;
const Thread = require('./Thread.js').Thread;
const SandboxStatus = require('./SandboxStatus.js').HandlerState;

const ThreadError = require('./ThreadError.js').ThreadError;

const __unitbase = require('./UnitBase.js');

const logger = require('./logger.js').mkLogger('scheduler');
const info = x => logger.info(x)
const debug = x => logger.debug(x)


const STACKDEPTH = 50;

let ProcessID = process.ProcessID;


const levels = require('./options.js')


let lub = levels.lub;

let TerminationStatus = {
    OK: 0,
    ERR: 1
}


class Scheduler {
    constructor(rt_uuid) {        
        this.rt_uuid = rt_uuid;

        this.__funloop = new Array()
        this.__blocked = new Array()
        this.__alive = {} // new Set();
        
        this.__currentThread = null; // current thread object

        this.stackcounter = 0;
                
        // the unit value 
        this.__unit = new LVal (__unitbase, levels.BOT);
    }

    done  (arg)  {            
        this.notifyMonitors();
        delete this.__alive [this.currentThreadId.val.toString()];              
    }


    halt  (arg, persist=null)  {
        this.raiseCurrentThreadPCToBlockingLev();
        let retVal = this.mkCopy(arg);
        this.notifyMonitors ();

        delete this.__alive[this.currentThreadId.val.toString()];            
        console.log(">>> Main thread finished with value:", retVal.stringRep());
        if (persist) {
            this.rtObj.persist (retVal, persist )
            console.log ("Saved the result value in file", persist)
        }
    }


    
    notifyMonitors (status = TerminationStatus.OK, errstr ) {
        
        let ids = Object.keys (this.__currentThread.monitors);
        for ( let i = 0; i < ids.length; i ++ ) {            
            let id = ids[i];
            let toPid = this.__currentThread.monitors[id].pid; 
            let refUUID = this.__currentThread.monitors[id].uuid; 
            let thisPid = this.__currentThread.tid;
            let statusVal = this.__currentThread.mkVal ( status ) ;
            let reason = TerminationStatus.OK == status ? statusVal : 
                this.rtObj.mkTuple ( [statusVal,  this.rtObj.mkVal (errstr)] );
            let message = this.rtObj.mkVal (this.rtObj.mkTuple ([ this.rtObj.mkVal("DONE"), refUUID, thisPid, reason]))             
            this.rtObj.sendMessageNoChecks ( toPid, message , false) // false flag means no need to return in the process
        }
    }

    raiseCurrentThreadPC (l)  {        
        this.__currentThread.raiseCurrentThreadPC(l);
    }
    
    raiseCurrentThreadPCToBlockingLev (l) {        
        this.__currentThread.raiseCurrentThreadPCToBlockingLev(l)
    }


    raiseBlockingThreadLev (l) {   
        this.__currentThread.raiseBlockingThreadLev(l); 
    }


    pinipush (l, cap) {        
        this.__currentThread.pinipush(l, cap)        
    }

    pinipop (cap) {
        return this.__currentThread.pinipop(cap); 
    }

    mkVal(x) {        
        return this.__currentThread.mkVal (x);    
    }
    
    mkValPos (x,p) {    
        return this.__currentThread.mkValPos (x,p);    
    }

    mkCopy (x) {
        return this.__currentThread.mkCopy (x);
    }

    mkBase(f,name=null) {
        return new LVal(new BaseFunction(f,name), levels.BOT);
    }

    initScheduler(node, stopWhenAllThreadsAreDone = false, stopRuntime = () => {}) {
        
        this.__node = node;
        this.__stopWhenAllThreadsAreDone = stopWhenAllThreadsAreDone;
        this.__stopRuntime = () => { stopRuntime () }
    }



    
    /** 
     * Returns the current thread's program counter
     * (updated on 2019-01-03; AA)
     */
    get pc() {        
        return this.__currentThread.pc;
    }

    /** 
     * Returns the curren thread's blocking lev
     */
    
    get blockingTopLev () {
        return this.__currentThread.blockingTopLev
    }

    /** 
     * Returns the join of the current pc with the blocking lev
     */

    get joinedLev () {
        return this.__currentThread.joinedLev;
    }

    get currentThreadId() {
        return this.__currentThread.tid;
    }

    set handlerState (st) {
        this.__currentThread.handlerState = st;        
    }

    get handlerState () {
        return this.__currentThread.handlerState;
    }

    resumeLoopAsync() {
        setImmediate(() => {this.loop()});
    }

    

    scheduleThreadT(t) {
        this.__funloop.push(t)
    }

  
    
    tail (thefun, arg1, arg2, nm) {
        this.__currentThread.tailInThread (thefun, arg1, arg2, nm)
        this.stepThread ()
    }
    

    returnInThread (arg) {        
        this.__currentThread.returnInThread(arg);
        this.stepThread ();
    }



    stepThread () {
        // console.log ( "FF ", this.__currentThread.theFun)
        if (this.stackcounter ++ < STACKDEPTH) {        
            this.__currentThread.next () ;
        } else {
            this.stackcounter = 0;            
            this.scheduleThreadT(this.__currentThread);                
        }
    }

    createNewProcessIDAtLevel(pcArg) {
        let pid = uuidv4();
        let pidObj = new ProcessID(this.rt_uuid, pid, this.__node);
        return new LVal(pidObj, pcArg);
    }

    scheduleNewThreadAtLevel (thefun, args, nm, levpc, levblock, ismain = false, persist=null) {
        let newPid = this.createNewProcessIDAtLevel(levpc);

        let halt = ismain ?  (arg)=> { this.halt (arg, persist) } : 
                             (arg) => { this.done (arg) };
        
        
        let t = new Thread 
            ( newPid
            , halt
            , thefun
            , args
            , nm
            , levpc
            , levblock
            , new SandboxStatus.NORMAL()
            , this.rtObj );


        this.__alive[newPid.val.toString()] = t;
        this.scheduleThreadT (t)
        return newPid;
    }

    schedule(thefun, args, nm) {
        this.__currentThread.runNext (thefun, args, nm);
        this.scheduleThreadT(this.__currentThread)
    }


    blockThread(t) {
        this.__blocked.push(t)
    }


    unblockThread(pid) {        
        for (let i = 0; i < this.__blocked.length; i++) {            
            if (process.pid_equals(this.__blocked[i].tid, pid)) {
                this.scheduleThreadT(this.__blocked[i]);
                this.__blocked.splice(i, 1);                
                break;
            }
        }
    }


    isAlive(tid) {
        return (this.__alive[tid.val.toString()] != null);
    }

    getThread (tid) {
        return this.__alive[tid.val.toString()];
    }


    setRuntimeObject (rtObj) {
        this.rtObj = rtObj;
    }



    /*****************************************************************************\

    2018-02-18: AA: a hypothesis about memory management in V8

    It appears that V8's memory management is not very well suited for infinitely
    running functions. In other words, functions are expected to eventually
    terminate, and all long-running computations are  expected to run through the
    event loop. This is not surprising given the application where V8 is used.
    This is why we periodically yield to the event loop; this hack appears to let
    GC claim the objects allocated throughout the runtime of this function.  Note
    that without this hack, we are observing memory leaks for many "server"-like
    programs; with the hack, we get a waivy memory consumption profile that reaches
    around 50M on the low points of the wave.

    \*****************************************************************************/

    loop() {
        // debug (`running scheduler loop with ${this.__funloop.length} many threads`)
        const $$LOOPBOUND = 5000;

        for (let $$loopiter = 0; $$loopiter < $$LOOPBOUND && (this.__funloop.length > 0); $$loopiter++) {

            let next = this.__funloop.shift();
            this.__currentThread = next;

            let theFun = next.theFun;

            try {  
                this.__currentThread.next();          
                
            } catch (e) {
                if (e instanceof ThreadError) {
                    // nothing to do in case it's just a thread error; 
                    // it's just important we do not resume
                    this.notifyMonitors(TerminationStatus.ERR, e.errstr) ;
                    delete this.__alive [this.currentThreadId.val.toString()];
                } else if ( e == "HandlerError") {
                    // we have an error inside of an receive pattern or guard;
                    // we are discarding the rest of the current thread and are
                    // scheduling the execution of the handler 

                    let f = this.handlerState.getTrapper().val; 
                    this.__currentThread.tailInThread (  f.fun, f.env, [], f.namespace ) ;
                    this.scheduleThreadT (this.__currentThread);
                } else  { // a real runtime error, must be a bug
                    console.log ("problems in the scheduler")
                    console.log (theFun);
                    throw e;
                }
            }
        }

        if (this.__funloop.length > 0) {
            // we are not really done, but are just hacking around the V8's memory management
            this.resumeLoopAsync();
        }

        if (this.__stopWhenAllThreadsAreDone && Object.keys(this.__alive).length == 0 ) {
            this.__stopRuntime();
        }
    }
}

module.exports = Scheduler;
