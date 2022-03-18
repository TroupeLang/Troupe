'use strict';
import { v4 as uuidv4} from 'uuid'
import { Thread } from './Thread';
import runId from './runId';
import { __unit } from './UnitVal';
import { mkTuple } from './ValuesUtil';
import { SchedulerInterface } from './SchedulerInterface';
import { RuntimeInterface } from './RuntimeInterface';
import { LVal } from './Lval.js'
import {ProcessID, pid_equals} from './process'
import SandboxStatus from './SandboxStatus'
import  {ThreadError, TroupeError} from './TroupeError'
import levels, {lub} from './options'
const yargs = require('yargs');
const showStack = yargs.argv.showStack
const logger = require('./logger.js').mkLogger('scheduler');
const info = x => logger.info(x)
const debug = x => logger.debug(x)

const STACKDEPTH = 150;

let TerminationStatus = {
    OK: 0,
    ERR: 1
}

export class Scheduler implements SchedulerInterface {
    rt_uuid: any;
    __funloop: Thread[];
    __blocked: any[];
    __alive: {};
    __currentThread: Thread;
    stackcounter: number;
    __unit: any;
    rtObj : RuntimeInterface
    __node: any;
    __stopWhenAllThreadsAreDone: boolean;
    __stopRuntime: () => void;    
    constructor(rtObj:RuntimeInterface) {        
        this.rt_uuid = runId;
        this.rtObj = rtObj
        this.__funloop = new Array()
        this.__blocked = new Array()
        this.__alive = {} // new Set();
        
        this.__currentThread = null; // current thread object

        this.stackcounter = 0;
                
        // the unit value 
        this.__unit = __unit 
    }


    resetScheduler() {
        // console.log (`The current length of __funloop is ${this.__funloop.length}`)
        // console.log (`The number of active threads is ${Object.keys(this.__alive).length}`)
        for (let x in this.__alive) {            
            if (this.currentThreadId.val.toString() == x) {
                // console.log (x, "ACTIVE")
            } else {
                // console.log (x, "KILLING");
                delete this.__alive[x]
            }
        }
        this.__blocked = []
        this.__funloop = [] 
        // console.log (`The number of active threads is ${Object.keys(this.__alive).length}`)
        // console.log (`The number of blocked threads is ${this.__blocked.length}`)
    }

    done  ()  {            
        this.notifyMonitors();
        // console.log (this.__currentThread.processDebuggingName, this.currentThreadId.val.toString(), "done")
        delete this.__alive [this.currentThreadId.val.toString()];              
    }


    halt  (persist=null)  {
        this.raiseCurrentThreadPCToBlockingLev();
        let retVal = new LVal (this.__currentThread.r0_val, 
                               lub(this.__currentThread.bl, this.__currentThread.r0_lev),
                               lub(this.__currentThread.bl, this.__currentThread.r0_tlev))

        this.notifyMonitors ();

        delete this.__alive[this.currentThreadId.val.toString()];            
        console.log(">>> Main thread finished with value:", retVal.stringRep());
        if (persist) {
            this.rtObj.persist (retVal, persist )
            console.log ("Saved the result value in file", persist)
        }
        return null;
    }
    
    notifyMonitors (status = TerminationStatus.OK, errstr = null) {
        let mkVal = this.__currentThread.mkVal
        let ids = Object.keys (this.__currentThread.monitors);
        for ( let i = 0; i < ids.length; i ++ ) {            
            let id = ids[i];
            let toPid = this.__currentThread.monitors[id].pid; 
            let refUUID = this.__currentThread.monitors[id].uuid; 
            let thisPid = this.__currentThread.tid;
            let statusVal = this.__currentThread.mkVal ( status ) ;
            let reason = TerminationStatus.OK == status ? statusVal : 
                mkTuple ( [statusVal,  mkVal (errstr)] );
            let message = mkVal (mkTuple ([ mkVal("DONE"), refUUID, thisPid, reason]))             
            this.rtObj.sendMessageNoChecks ( toPid, message , false) // false flag means no need to return in the process
        }
    }

    raiseCurrentThreadPC (l)  {        
        this.__currentThread.raiseCurrentThreadPC(l);
    }
    
    raiseCurrentThreadPCToBlockingLev () {        
        this.__currentThread.raiseCurrentThreadPCToBlockingLev()
    }


    raiseBlockingThreadLev (l) {   
        this.__currentThread.raiseBlockingThreadLev(l); 
    }


    pinipush (l, cap) {        
        this.__currentThread.pcpinipush(l, cap)        
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


    initScheduler(node, stopWhenAllThreadsAreDone = false, stopRuntime = () => {}) {        
        this.__node = node;
        this.__stopWhenAllThreadsAreDone = stopWhenAllThreadsAreDone;
        this.__stopRuntime = stopRuntime
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

    

    scheduleThread(t) {
        this.__funloop.push(t)
    }


    createNewProcessIDAtLevel(pcArg) {
        let pid = uuidv4();
        let pidObj = new ProcessID(this.rt_uuid, pid, this.__node);
        return new LVal(pidObj, pcArg);
    }

    scheduleNewThreadAtLevel (thefun, arg, levpc, levblock, ismain = false, persist=null) {
        let newPid = this.createNewProcessIDAtLevel(levpc);

        let halt = ismain ?  ()=> { this.halt (persist) } : 
                             () => { this.done () };
        
        
        let t = new Thread 
            ( newPid
            , halt
            , thefun
            , arg
            , levpc
            , levblock
            , new SandboxStatus.NORMAL()
            , this.rtObj
            , this );


        this.__alive[newPid.val.toString()] = t;
        this.scheduleThread (t)
        return newPid;
    }

    schedule(thefun, args, nm) {
        this.__currentThread.runNext (thefun, args, nm);
        this.scheduleThread(this.__currentThread)
    }


    blockThread(t) {
        this.__blocked.push(t)
    }


    unblockThread(pid) {        
        for (let i = 0; i < this.__blocked.length; i++) {            
            if (pid_equals(this.__blocked[i].tid, pid)) {
                this.scheduleThread(this.__blocked[i]);
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


    stopThreadWithErrorMessage (t:Thread, s:string ) {
        this.notifyMonitors(TerminationStatus.ERR, s) ;
        delete this.__alive [t.tid.val.toString()];
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


    loop()  {
        const $$LOOPBOUND = 500000;
        let _FUNLOOP = this.__funloop
        let _curThread: Thread; 
        let dest; 
        try {
            for (let $$loopiter = 0; $$loopiter < $$LOOPBOUND && _FUNLOOP.length > 0; $$loopiter ++ ) {
                _curThread = _FUNLOOP.shift();
                this.__currentThread = _curThread;
                dest = _curThread.next 
                let ttl = 1000;  // magic constant; 2021-04-29
                while (dest && ttl -- ) {
                    // if (showStack) { // 2021-04-24; AA; TODO: profile the addition of this conditional in this tight loop
                    //     this.__currentThread.showStack()
                    // }
                    // console.log (">>>>>>>>>>")
                    // console.log (dest.toString())
                    // console.log ("<<<<<<<<<<")
                    // if (dest.debugname ) {
                    //     console.log (" -- ", dest.debugname)
                    // }
                    dest = dest ()
                }

                if (dest) {
                    _curThread.handlerState.checkGuard() 

                    _curThread.next = dest ;
                    _FUNLOOP.push (_curThread);
                }
            }    
        } catch (e) {
            if (e instanceof TroupeError) {
                e.handleError(this);
            } else {
                console.log ("--- Schedule module caught an internal exception ---")
                console.log ("--- The following output may help identify a bug in the runtime ---")
                console.log ("Destination function\n" , dest)
                this.__currentThread.showStack()
                throw e;
            }
        }

        if (_FUNLOOP.length > 0) {
            // we are not really done, but are just hacking around the V8's memory management
            this.resumeLoopAsync();
        }
  
        if (this.__stopWhenAllThreadsAreDone && Object.keys(this.__alive).length == 0 ) {
            this.__stopRuntime();
        }
    }
    
}