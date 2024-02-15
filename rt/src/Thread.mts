import * as levels from './options.mjs'
import { LVal, LValCopyAt } from './Lval.mjs';
import { HandlerError, ImplementationError, StrThreadError } from './TroupeError.mjs';
import yargs from 'yargs';
let logLevel = yargs.argv.debug? 'debug' : 'info'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('thread',  logLevel);
const debug = x => logger.debug(x)
let lub = levels.lub;
let flowsTo = levels.flowsTo
import { v4 as uuidv4} from 'uuid'

import { TroupeType } from './TroupeTypes.mjs'
import { RuntimeInterface } from './RuntimeInterface.mjs';
import { __unit } from './UnitVal.mjs';
import { Level } from './Level.mjs';
import { SchedulerInterface } from './SchedulerInterface.mjs';
import { getRuntimeObject } from './SysState.mjs';
import { HnState } from './SandboxStatus.mjs';


let isPiniMode = yargs.argv.pini?true:false;


export enum PCDeclassificationPurpose {
    Full="pcpush", 
    Pini="pinipush"
}

// stack frame configuration constants
export const CALLSIZE = 5
const SPOFFSET = CALLSIZE
const PCOFFSET  = 4
export const RETOFFSET = 3 
const MCLEAROFFSET = 2
const BRANCHFLAGOFFSET = 1

const BRANCH_FLAG_OFF = false
const BRANCH_FLAG_ON  = true 



export class Capability<T> {
    /*
    
    Linked capabilities with payload. 
    
    The "token" aspect of the capability is represented as a string. These capabilites
    carry payload data that can be used by the runtime upon their successful check.
    This is useful because the runtime does not need to carry the data itself, and just
    rely on the checks of the tokens. A special aspect of the payload is that 
    they carry a link to the previous capability, and this is represented in the `priv` 
    field of this class. 


    We use these kinds of capabilities to enforce a scoping discipline on pcpush/pinipush
    and raise/lower mailbox mechanisms.

    */
   
    _troupeType: TroupeType
    uid : string 
    data : T
    prev: string
    dataLevel: Level
    constructor (c: string, l:T,p: string , dataLevel ) {
        this.uid = c; 
        this.data = l;
        this.prev = p // provides scoping control; needs better name; AA; 2020-02-08
        this.dataLevel = dataLevel
        this._troupeType = TroupeType.CAPABILITY
    }
    stringRep() : string {
        return this.uid;
    }
}

class  MboxClearance {
  boost_level: any; 
  pc_at_creation: any; 
  constructor (lclear:any, pc:any) {
    this.boost_level = lclear;
    this.pc_at_creation = pc;
  }

  stringRep () {
    return this.boost_level.stringRep ()
  }

  
}


class Mailbox extends Array {
    mclear : MboxClearance ;
    caps : string;

    peek_cache_index : number 
    peek_cache_position: number 
    peek_cache_lowb  : Level 
    peek_cache_highb : Level 
    

    constructor () {
        super ()
        this.mclear = new MboxClearance (levels.BOT, levels.BOT);
        this.caps = null;

        this.peek_cache_index = null; 
        this.peek_cache_position = null;    
        this.peek_cache_lowb  = null; 
        this.peek_cache_highb = null
    }
    newMessage (x) {
        this.push(x);
    }


    resetPeekCache ()  {
        this.peek_cache_index = null;
        this.peek_cache_lowb  = null; 
        this.peek_cache_position = null;    
        this.peek_cache_lowb  = null; 
        this.peek_cache_highb = null
    }

}

class ThreadState {
  callStack: any [] 
  constructor (s: any []) {
     this.callStack = s
  }
}


export class SleepTimeout {    
    sleepingUntil  : number 
    timeoutObject  : any;
    resumeThread   : Thread
    timeLeft       : number;
    constructor(delay,  th) {        
        this.timeoutObject = setTimeout( () => {this.afterTimeout()}, delay);
        this.sleepingUntil = Date.now () + delay 
        this.resumeThread = th
        this.timeLeft = null;
    }

    pause () {
        clearTimeout(this.timeoutObject)
        this.timeLeft = Math.max (0, this.sleepingUntil - Date.now())
    }

    resume (newThread) { 
        this.resumeThread = newThread;
        this.sleepingUntil = Date.now() + this.timeLeft;
        this.timeoutObject = setTimeout (() => {this.afterTimeout()}, this.timeLeft)
        this.timeLeft = null;
    }

    resetTimeout(newDelay,newThread) {
        this.resumeThread = newThread;
        this.timeoutObject = setTimeout (() => {this.afterTimeout()}, newDelay)
        this.sleepingUntil = Date.now() + newDelay
    }

    afterTimeout () {
        let theThread = this.resumeThread;
        theThread.sleepTimeout = null; 
        theThread.returnSuspended (__unit);
        let sched = getRuntimeObject().__sched
        sched.scheduleThread (theThread)
        sched.resumeLoopAsync()
    }    
}

export class Thread {
    tid: any;    
    pc: Level;
    bl: Level;


    // registers 
    r0_val: any;
    private _r0_lev: any;
    public get r0_lev(): any {
        return this._r0_lev;
    }
    public set r0_lev(value: any) {
        if (!value?.isLevel ) {
            console.log ("RO-LEV debugging")
            console.log ( (new Error().stack) )
            this.showStack()
        }
        this._r0_lev = value;
    }
    r0_tlev: any;

    pini_uuid : string;

    handlerState: HnState;
    monitors: {};
    killCounter: number;
    // sleeping: boolean;
    // sleepingUntil: number;
    // wakeupFn: () => void;
    // timeoutObject: any;
    sleepTimeout : SleepTimeout
    rtObj: RuntimeInterface;
    sched : SchedulerInterface;
    mailbox: Mailbox;
    next :  () => any;
    callStack : any []
    _sp : number;
    boundSlot : number;
    
    _isDataBoundByPC: boolean = false;
    
    processDebuggingName: string;

    failureRate: number  = 0
    failureStartTime : number = 0

    get sleeping () {
        return (this.sleepTimeout != null);
    } 
    
    constructor(tid, ret, theFun, arg, pc, levblock, handlerState, rtObj, sched) {
        this.tid = tid;    
        this.pc = pc;
        this.bl = levblock;
        this.pini_uuid = null;
        this.handlerState = handlerState;
        this.monitors = {};
        this.killCounter = 0;
        this.sleepTimeout = null; // no sleep command        
        // this.sleeping = false;
        // this.timeoutObject = null;
        this.rtObj = rtObj;
        this.sched = sched;
        this.mailbox = new Mailbox();
        this.processDebuggingName = null;         
        
        /* 

        The call frames have the structure



        ---> stack growth direction --->

        |---------+-------------------+--------------+-----------------------------+---------------+-------------------------+--------------------|
        | sp_prev | pc at return site | ret callback | mclear at the time of entry | branching bit | [escaping locals]       | bound_slot         |
        |---------+-------------------+--------------+-----------------------------+---------------+-------------------------+--------------------|
        | sp - 5  | sp - 4            | sp - 3       | sp - 2                      | sp - 1        | sp ... (sp + framesize) | sp + framesize + 1 |

        
        The branching bit indicates whether the execution of this frame invoked any branch 
        instructions. Upon returns we check whether the flag is set, and in that case 
        we enforce that the current mailbox clearance must match the one at the time of 
        the invocation.

        -- AA; 2020-02-12 

        prev_sp | pc_at_ret_point | ret_cb | mclear | branch_bit |     <... locals ... > 
                                                                ^
                                                                |
                                                                |
                                                                sp
       
        */ 
       
        
        /*                                                                
        this.callStack = [ 0, pc, null, null, BRANCH_FLAG_OFF
                         , 5, pc, ret, this.mailbox.mclear, BRANCH_FLAG_OFF] 
                                            // auxiliary bottom element of the call stack; never called
                                            // but is convenient for keeping track of the PC 
        this._sp = CALLSIZE * 2
        */

        this.callStack = [ 0, pc, ret, this.mailbox.mclear, BRANCH_FLAG_OFF]                              
        this._sp = CALLSIZE 
        // let arg0 = theArgs [0]
        // let arg1 = theArgs [1]
        this.r0_val = arg.val 
        this.r0_lev = arg.lev 
        this.r0_tlev = arg.tlev 
        this.next = theFun
        // () => {            
        //     return theFun();
        // }           
    }


    exportState ()  {
        let __state = {            
            pc             : this.pc,
            bl             : this.pc,
            pini_uuid      : this.pini_uuid,
            sp             : this._sp,
            next           : this.next,            
            callStack      : this.callStack,
            r0_val         : this.r0_val,
            r0_lev         : this.r0_lev,
            r0_tlev        : this.r0_tlev  
            
        }
        return __state;
    }

    importState (__state) {             
        this.pc =        __state.pc
        this.bl =        __state.bl  
        this.pini_uuid = __state.pini_uuid
        this._sp =       __state.sp
        this.next =      __state.next
        this.callStack = __state.callStack
        this.r0_val =    __state.r0_val          
        this.r0_lev =    __state.r0_lev         
        this.r0_tlev =   __state.r0_tlev 
    }

    resetStackForSandboxing () {
      this.callStack = []
      this._sp = 0
    }


    showStack ()  {
        console.log ("======== SHOW STACK ========= ")
        console.log (`sp = ${this._sp} boundslot = ${this.boundSlot}`)
        let j = this._sp - 1 
        let stack = this.callStack
        while ( j > 0) {
            console.log (`-${j.toString().padStart(5,'-')} branch bit: ${stack[j--]}`)
            let mclear = stack[j]            
            console.log (` ${j.toString().padStart(5,' ')} mclear    : ${mclear?.stringRep()}`)
            j -- 
            let ret = stack [j]
            let ret_string = ret?.debugname 
            if (!ret_string) {
                ret_string = ret?.toString ()
            }
            
            console.log (` ${j.toString().padStart(5,' ')} ret       : ${ret_string}`)
            j --
            console.log (` ${j.toString().padStart(5,' ')} pc_ret    : ${stack[j]?.stringRep()}`)
            j --
            console.log (` ${j.toString().padStart(5,' ')} sp_prev   : ${stack[j]}`)
            let sp_prev = stack[j];
            j = sp_prev - 1 ;
        }
    }

    


    addMonitor (pid, r) {        
        this.monitors[r.val] = {pid: pid, uuid: r} 
    }

    tailCall (f, x) {
        this.setR0ToLValue (x);
        return f;
    }


    invalidateSparseBit () {
        this.callStack[this.boundSlot] = false;
    }

    // Check whether the label of R0 (argument), the data level of R0 and the given one are bound by PC.
    checkDataBoundsEntry (x: Level) {
        const _pc = this.pc 
        let y = 
             flowsTo(this.r0_lev, _pc) 
              && 
                flowsTo (x, _pc)
                    && (this.r0_val._troupeType == undefined 
                                ? true
                                : flowsTo (this.r0_val.dataLevel, _pc)
                        )
             
                                    
        // this._isDataBoundByPC = y;    
        return y;
    }

    // Check whether the label of R0 (return value) and the data level of R0 are bound by PC.
    // Return false if x is false.
    // TODO Better check x directly and do not call this function if false (now that _isDataBoundByPC is not updated).
    checkDataBounds (x: boolean) {
        const _pc = this.pc 
        let y = 
             x? flowsTo(this.r0_lev, _pc) 
                    && (this.r0_val._troupeType == undefined 
                                ? true
                                : flowsTo (this.r0_val.dataLevel, _pc)
                        )
             : false 
                                    
        // this._isDataBoundByPC = y;    
        return y;
    }

    


    runNext (theFun, args, nm)  {
        this.next = () => {
            theFun.apply (nm, args);
        }
    }

    
   

    block(cb) {
        this.next = cb //  () => { return cb(); }
    }

    get arg_as_lval () {
        return new LVal (this.r0_val, this.r0_lev, this.r0_tlev);
    }

    pushFrame (cb, framesize=0) {
        // console.log ("CALL", this._sp, this.r0_val, framesize )
        let _prev_sp = this._sp 
        this._sp = this._sp + framesize + CALLSIZE
        this.callStack[this._sp - SPOFFSET] = _prev_sp
        this.callStack[this._sp - PCOFFSET] = this.pc 
        this.callStack[this._sp - RETOFFSET] = cb 
        this.callStack[this._sp - MCLEAROFFSET] = this.mailbox.mclear 
        this.callStack[this._sp - BRANCHFLAGOFFSET] = BRANCH_FLAG_OFF

//        this.callStack.push (this.pc)
//        this.callStack.push ( cb ) 
//        this.callStack.push ( this.mailbox.mclear )
//        this.callStack.push (BRANCH_FLAG_OFF) 
//        this._sp += FRAMESIZE;

    }

    setBranchFlag () {
        this.callStack[this._sp - BRANCHFLAGOFFSET] = BRANCH_FLAG_ON
    }
    
    returnSuspended (arg) {       
        // console.log("RET", this._sp)
        let rv = new LValCopyAt (arg, this.pc);
        this.next = () => {            
            return this.returnImmediateLValue (rv);
        }
    }

    setR0ToLValue (arg:LVal) {
        this.r0_val = arg.val
        this.r0_lev = arg.lev
        this.r0_tlev = arg.tlev
    }

    returnImmediateLValue (arg:LVal) {
        this.r0_val = arg.val
        this.r0_lev = arg.lev
        this.r0_tlev = arg.tlev
        return this.returnImmediate()
    }

    returnImmediate () {            
        let _STACK = this.callStack  
        let _SP = this._sp 

        let branchFlag = _STACK[_SP  - BRANCHFLAGOFFSET]
        let lclear = _STACK[_SP - MCLEAROFFSET]
        let ret = _STACK[_SP - RETOFFSET]

        // 2021-03-23; AA -- hack: in PINI mode the blocking label is restored back to the pc upon returns
        if (isPiniMode) {
            this.bl = this.pc
        }

        if (branchFlag) {
            if (lclear != this.mailbox.mclear) {
                this.threadError (`Mailbox clearance label is not restorted after being raised in a branch; stack depth = ${this._sp}` )
            }
        }
        this.pc  = _STACK [_SP - PCOFFSET]; 
        this._sp = _STACK [_SP - SPOFFSET]; 
        this.next = ret        
        return ret;
    }

    
    mkUuidVal () {
        let pid = uuidv4();
        let uuidval = this.mkVal ( pid );
        return uuidval;  
    }  


    pcpinipush ( auth: any, purpose: PCDeclassificationPurpose | string, bl = this.bl )  {
        let uid = uuidv4()
        let cap = this.mkVal (new Capability(uid,
                    { bl
                    , pc: this.pc
                    , auth : auth                    
                    , purpose: purpose
                    },
                    this.pini_uuid,
                    this.pc)); // 2021-05-12; AA; TODO: revisit this; alternative might be te use auth level? 
                               // also, why not block though that will require invalidating the
                               // sparse bit
                
        this.pini_uuid = uid;
        return this.returnImmediateLValue(cap)
    }


    pcpop (cap_lval) {
        if (this.pini_uuid == null) {
            this.threadError ("unmatched pcpop");
        }
       
        let cap: Capability<any> = cap_lval.val;        
        let {bl, pc, auth, purpose} = cap.data;
        
        // check the capability
        if (this.pini_uuid != cap.uid || purpose != PCDeclassificationPurpose.Full) {
            this.threadError ("Ill-scoped pinipush/pinipop");
            return null; // does not schedule anything in this thread 
                         // effectively terminating/blocking the thread
        }

        
        // We declassify the current blocking level to the old blocking level. 
        // and also the current pc to the old pc. 
        // We check that there is sufficient authority to declassify from 
        // the current blocking level all the way down to the target pc 

        let levFrom = this.bl;
        let levTo = pc


        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // check that the provided authority is sufficient for the declassification
        let ok_to_declassify = 
            levels.flowsTo (levFrom, levels.lub ( auth.val.authorityLevel, levTo ));
        if (ok_to_declassify) {        
            this.pc = pc;           
            this.bl = bl;

            // declassify the call stack...             
            let loop_sp = this._sp 
            let j = loop_sp - PCOFFSET; 
 
            while (j >= 0 && !levels.flowsTo (this.callStack[j], pc)) {   
                this.callStack[j] = pc;
                loop_sp = this.callStack[loop_sp - SPOFFSET]
                j = loop_sp - PCOFFSET 
            //   throw new Error ("revisit") //FRAMESIZE;
            }            

            // make sure we can restore earlier caps
            this.pini_uuid = cap.prev;
            // return in the thread through the scheduler
            return this.returnImmediateLValue (__unit);                        
        } else {
            this.threadError ( "Not enough authority for pini declassification\n" + 
                            ` | from level of the blocking level: ${levFrom.stringRep()}\n` +
                            ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n`  +
                            ` | to level of the blocking level: ${levTo.stringRep()}`);
        }                
    }
    


    pinipop (cap_lval) {
        if (this.pini_uuid == null) {
            this.threadError ("unmatched pinipop");
        }

        debug (`Current pc level is ${this.pc.stringRep()}`)

        this.raiseBlockingThreadLev(this.pc); // maintaining the invariant that the blocking level is as high as the pc level       
        
        let cap: Capability<any> = cap_lval.val;        
        let {bl, pc, auth, purpose} = cap.data;
        

        if (this.pini_uuid != cap.uid || purpose != PCDeclassificationPurpose.Pini) {            
            this.threadError ("Ill-scoped pinipush/pinipop");
            return; // does not schedule anything in this thread 
                    // effectively terminating the thread
        }

        // If we are here then the pinipop is well-scoped
        // so we check the declassifications now

        let levFrom = this.bl;
        let levTo = bl;

        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // this.showStack()
        // check that the provided authority is sufficient to perform declassification to the next level
        let ok_to_declassify = 
            levels.flowsTo (levFrom, levels.lub ( auth.val.authorityLevel, levTo ));
        if (ok_to_declassify) {
            this.bl = levTo ; 
            this.pini_uuid = cap.prev;
            return this.returnImmediateLValue (__unit);                        
        } else {
            this.threadError ( "Not enough authority for pini declassification\n" + 
                            ` | from level of the blocking level: ${levFrom.stringRep()}\n` +
                            ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n`  +
                            ` | to level of the blocking level: ${levTo.stringRep()}`);
        }        
    }


    blockdeclto (auth, bl_to = this.pc) {        
        let is_bounded_by_pc = flowsTo (this.pc, bl_to);
        if (!is_bounded_by_pc) {
            this.threadError ("The provided target blocking level is lower than the current pc\n" + 
                              ` | the current pc: ${this.pc.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`)
            return; // should be unnecessary
        }
        

        let ok_to_use = 
            levels.flowsTo (auth.lev, bl_to);
        if (!ok_to_use) {
            this.threadError ("The provided authority value is tainted\n" + 
                              ` | the level of the authority value: ${auth.lev.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`)
            return; // should be unnecessary
        }
        let ok_to_declassify = 
            levels.flowsTo (this.bl, levels.lub (auth.val.authorityLevel, bl_to))
        if (ok_to_declassify) {
            this.bl = bl_to; // the actual downgrade
        } else {
            this.threadError ("Not enough authority for blocking level declassification\n" + 
                              ` | provided authority: ${auth.val.authorityLevel.stringRep()}\n` +
                              ` | current blocking level: ${this.bl.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}\n`
                              )
        }
        return this.returnImmediateLValue (__unit); 

    }

    raiseBlockingThreadLev (l) {                
        this.bl = lub (this.bl, l)        
    }

    raiseCurrentThreadPCToBlockingLev () {        
        this.pc = lub(this.pc, this.bl ) ;
    }

    raiseCurrentThreadPC (l)  {        
        this.pc = lub( this.pc, l )        
        this.raiseBlockingThreadLev(this.pc); 
            // 2018-11-29: AA; observe that we are raise the blocking level
            // automaticaly every time we raise the PC level.
    }



    mkVal(x) {
        return new LVal(x, this.pc, this.pc );
    }

    mkValPos(x: any, pos: string) {
        return new LVal (x, this.pc, this.pc, pos);
    }

    /*
    mkValWithTroupeType(x, t) {
        return new LVal (x, this.pc, this.pc, null, t)
    }
    */

    mkValWithLev(x:any, l:any) {                    
        return new LVal ( x
                        , lub(this.pc, l)
                        , this.pc )              
    }

    mkCopy (x) {
        return new LValCopyAt (x, this.pc);
        // return new LVal(x.val, lub(x.lev, this.pc), lub (x.tlev, this.pc) )
    }

    
    printPc () {
        console.log ("PC:", this.pc.stringRep());
        console.log ("BL:", this.bl.stringRep());
    }


    tidErrorStringRep() {
        if (this.processDebuggingName) {
            return ("[" + this.processDebuggingName + "]" + this.tid.stringRep() )
        }   
        else {
            return this.tid.stringRep()
        }
    }


    threadError (s:string, internal = false) {
        if ( this.handlerState.isNormal()) {  
          if (internal)  {
            throw new ImplementationError(s)
          }
          else {
            throw new StrThreadError(this, s);
          }
        } else {
          this.raiseCurrentThreadPC(this.handlerState.lev);
          throw new HandlerError (this, s) //  "HandlerError" 
        }
    }
    
    
    addMessage (message) {
        this.mailbox.newMessage (message);    
    }

    raiseMboxClearance (new_lclear: any) {        
        /*
        if (!flowsTo(this.pc, this.mailbox.lclear)) {
            this.threadError( `Cannot raise mailbox clearance level in a high context\n` + 
                              `| current thread's pc level: ${this.pc.stringRep()}\n` +
                              `| current mailbox clearance level: ${this.mailbox.lclear.stringRep()}`)
            return;
        } */

        let uid = uuidv4() ;
        let cap = this.mkVal (new Capability(uid, this.mailbox.mclear, this.mailbox.caps, this.pc)) 
        this.mailbox.caps = uid;
        this.mailbox.mclear = new MboxClearance(lub (new_lclear.val, this.mailbox.mclear.boost_level), this.pc);

        // this.returnSuspended( cap ); 
        // this.sched.stepThread();         
        return this.returnImmediateLValue(cap)
    }

    lowerMboxClearance (cap_lval:any, auth:any) {
        if (this.mailbox.caps == null ) {
            this.threadError ("unmatched lowering of mailbox clearance")
            return;
        }
        
        let cap:Capability<MboxClearance> = cap_lval.val 

        if (this.mailbox.caps != cap.uid ) {            
            this.threadError ("Ill-scoped raise/lower of mailbox clearance:\n" + 
                              `expected cap: ${this.mailbox.caps}\n` + 
                              `provided cap: ${cap.uid}`)
            return;
        }


        // since we are going to update the level of the current mailbox label
        // we have to check that we do not affect it in a high context
        // note: the intuition here follows the principle of non-sensitive upgrade
        // 2020-02-12:AA

        if (!levels.flowsTo (this.pc , this.mailbox.mclear.pc_at_creation)) {
            this.threadError ("Cannot lower mailbox when the pc more sensitive than the mailbox clearance level\n" +
                              `| current thread's pc level: ${this.pc.stringRep()}\n` +                              
                              `| mailbox clearance level: ${this.mailbox.mclear.pc_at_creation.stringRep()}`)
            
        }

        // check the authority is sufficient to downgrade from the current boost 
        // to the target one 
        let ok_to_lower = levels.flowsTo (this.mailbox.mclear.boost_level, lub (auth.val.authorityLevel, cap.data.boost_level))
        
        if (!ok_to_lower) {
            this.threadError("Insufficient authority for lowering the mailbox clearance\n" +
                            `| authority provided: ${auth.val.stringRep()}\n` +
                            `| current level of the mailbox: ${this.mailbox.mclear.boost_level.stringRep()}\n` +
                            `| target level of the mailbox: ${cap.data.boost_level.stringRep()}`
            )
            return;
        }            

        this.mailbox.mclear = cap.data; // restoring the clearance level
        this.mailbox.caps = cap.prev;
        return this.returnImmediateLValue(__unit) 
        // this.returnSuspended(__unit)
        // this.sched.stepThread();         
    }
}

