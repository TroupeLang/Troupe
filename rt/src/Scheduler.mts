'use strict';
import { v4 as uuidv4} from 'uuid'
import { Thread } from './Thread.mjs';
import { FifoQueue } from './FifoQueue.mjs';
import runId from './runId.mjs';
import { __unit } from './UnitVal.mjs';
import { mkTuple } from './ValuesUtil.mjs';
import { SchedulerInterface } from './SchedulerInterface.mjs';
import { RuntimeInterface } from './RuntimeInterface.mjs';
import { LVal } from './Lval.mjs'
import {ProcessID} from './process.mjs'
import SandboxStatus from './SandboxStatus.mjs'
import  {ThreadError, TroupeError} from './TroupeError.mjs'
import  {lub} from './Level.mjs'
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { sendSocketMessage, isResultSocketEnabled } from './resultSocket.mjs';

import {SYSTEM_PROCESS_STRING} from './Constants.mjs'
const argv = getCliArgs();

import { mkLogger } from './logger.mjs'
const logger = mkLogger('scheduler');
const info = x => logger.info(x)
const debug = x => logger.debug(x)

const STACKDEPTH = 150;

let TerminationStatus = {
    OK: 0,
    ERR: 1
}

export class Scheduler implements SchedulerInterface {
    rt_uuid: any;
    // Runnable threads, in round-robin order.
    __funloop: FifoQueue<Thread>;
    // Threads parked in receive, keyed by pid string, so that message
    // delivery unblocks the recipient in O(1) instead of scanning.
    __blocked: Map<string, Thread>;
    __alive: Map<string, Thread>;
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
        this.__funloop = new FifoQueue()
        this.__blocked = new Map()
        this.__alive = new Map()
        
        this.__currentThread = null; // current thread object

        this.stackcounter = 0;
                
        // the unit value 
        this.__unit = __unit 
    }


    // Both __alive and __blocked are keyed by this string form of a pid.
    private pidKey (tid) : string {
        return tid.val.toString();
    }

    resetScheduler() {
        let current = this.pidKey(this.currentThreadId)
        for (let x of this.__alive.keys()) {
            if (current != x) {
                this.__alive.delete(x)
            }
        }
        this.__blocked = new Map()
        this.__funloop.clear()  // in place: loop() holds an alias to the queue
    }

    done  ()  {
        this.notifyMonitors();
        this.__alive.delete (this.pidKey(this.currentThreadId));
    }


    halt  (persist=null)  {
        this.raiseCurrentThreadPCToBlockingLev();
        let retVal = new LVal (this.__currentThread.r0_val, 
                               lub(this.__currentThread.bl, this.__currentThread.r0_lev),
                               lub(this.__currentThread.bl, this.__currentThread.r0_tlev))

        this.notifyMonitors ();

        this.__alive.delete(this.pidKey(this.currentThreadId));
        sendSocketMessage({ type: 'main-thread-result', value: retVal.stringRep() });
        if (!argv[TroupeCliArg.SuppressMainThreadFinishedMessage] && !isResultSocketEnabled()) {
            console.log(">>> Main thread finished with value:", retVal.stringRep());
        }
        if (persist) {
            this.rtObj.persist (retVal, persist )
            console.log ("Saved the result value in file", persist)
        }
        return null;
    }
    
    notifyMonitors (status = TerminationStatus.OK, errstr = null) {
        let t = this.__currentThread
        let ids = Object.keys (t.monitors);
        for ( let i = 0; i < ids.length; i ++ ) {
            let id = ids[i];
            let toPid = t.monitors[id].pid;
            let refUUID = t.monitors[id].uuid;
            let thisPid = t.tid;
            let statusVal = t.mkVal ( status ) ;
            let reason = TerminationStatus.OK == status ? statusVal :
                t.mkVal (mkTuple ( [statusVal,  t.mkVal (errstr)] ));
            let message = t.mkVal (mkTuple ([ t.mkVal("DONE"), refUUID, thisPid, reason]))
            this.rtObj.sendMessageNoChecks ( toPid, message , undefined, false) // false flag means no need to return in the process
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
        this.__funloop.enqueue(t)
    }


    createNewProcessIDAtLevel(pcArg, isSystem = false) {
        let pid = isSystem ? SYSTEM_PROCESS_STRING : uuidv4();
        let pidObj = new ProcessID(this.rt_uuid, pid, this.__node);
        return new LVal(pidObj, pcArg);
    }



    scheduleNewThreadAtLevel (thefun, arg, levpc, levblock, ismain = false, persist=null, isSystem = false) {
        let newPid = this.createNewProcessIDAtLevel(levpc, isSystem);

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


        this.__alive.set(this.pidKey(newPid), t);
        this.scheduleThread (t)
        return newPid;
    }

    schedule(thefun, args, nm) {
        this.__currentThread.runNext (thefun, args, nm);
        this.scheduleThread(this.__currentThread)
    }


    blockThread(t) {
        this.__blocked.set(this.pidKey(t.tid), t)
    }


    unblockThread(pid) {
        let key = this.pidKey(pid)
        let t = this.__blocked.get(key)
        if (t != null) {
            this.__blocked.delete(key)
            this.scheduleThread(t)
        }
    }


    isAlive(tid) {
        return (this.__alive.get(this.pidKey(tid)) != null);
    }

    getThread (tid) {
        return this.__alive.get(this.pidKey(tid));
    }


    stopThreadWithErrorMessage (t:Thread, s:string ) {
        this.notifyMonitors(TerminationStatus.ERR, s) ;
        this.__alive.delete (this.pidKey(t.tid));
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
            for (let $$loopiter = 0; $$loopiter < $$LOOPBOUND && !_FUNLOOP.isEmpty; $$loopiter ++ ) {
                _curThread = _FUNLOOP.dequeue();
                this.__currentThread = _curThread;
                dest = _curThread.next 
                let ttl = 1000;  // magic constant; 2021-04-29
                while (dest && ttl -- ) {
                    dest = dest ()
                }

                if (dest) {
                    _curThread.handlerState.checkGuard() 

                    _curThread.next = dest ;
                    _FUNLOOP.enqueue (_curThread);
                }
            }    
        } catch (e) {
            if (e instanceof TroupeError) {
                e.handleError(this);
            } else {
                console.error ("--- Schedule module caught an internal exception ---")
                console.error ("--- The following output may help identify a bug in the runtime ---")
                console.error ("Destination function\n" , dest)
                this.__currentThread.showStack()
                throw e;
            }
        }

        if (!_FUNLOOP.isEmpty) {
            // we are not really done, but are just hacking around the V8's memory management
            this.resumeLoopAsync();
        }

        if (this.__stopWhenAllThreadsAreDone && this.__alive.size == 0 ) {
            this.__stopRuntime();
        }
    }
    
}