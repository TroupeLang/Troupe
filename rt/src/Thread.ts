import levels = require ('./options')
import { LVal } from './Lval.js';
import { ThreadError } from './ThreadError.js';
import colors = require('colors/safe');
import { StackItem } from './StackItem';
import { StackCallItem } from './StackCallItem';
const Authority = require ('./Authority.js').Authority;
const logger = require('./logger.js').mkLogger('thread');
const debug = x => logger.debug(x)
let lub = levels.lub;
let flowsTo = levels.flowsTo
import uuidv4 = require('uuid/v4');
import { Asserts  } from './Asserts'

export enum PCDeclassificationPurpose {
    Full="pcpush", 
    Pini="pinipush"
}

// stack frame configuration constants
const FRAMESIZE = 4
const PCOFFSET  = FRAMESIZE - 1 // pc is located at the first position
const BRANCHFLAGOFFSET = 0 
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
   
    uid : string 
    data : T
    prev: string
    constructor (c: string, l:T,p: string ) {
        this.uid = c; 
        this.data = l;
        this.prev = p // provides scoping control; needs better name; AA; 2020-02-08
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

    constructor () {
        super ()
        this.mclear = new MboxClearance (levels.BOT, levels.BOT);
        this.caps = null;
    }
    newMessage (x) {
        this.push(x);
    }
}


export class Thread {
    tid: any;    
    pc: any;
    bl: any;

    pini_uuid : string;

    handlerState: any;
    monitors: {};
    killCounter: number;
    sleeping: boolean;
    timeoutObject: any;
    rtObj: any;
    sched : any;
    mailbox: Mailbox;
    next :  () => void;
    callStack : any []
    _sp : number;

    processDebuggingName: string;


    _asserts: Asserts

    
    constructor(tid, ret, theFun, theArgs, namespace, pc, levblock, handlerState, rtObj, sched) {
        this.tid = tid;    
        this.pc = pc;
        this.bl = levblock;
        this.pini_uuid = null;
        this.handlerState = handlerState;
        this.monitors = {};
        this.killCounter = 0;
        this.sleeping = false;
        this.timeoutObject = null;
        this.rtObj = rtObj;
        this.sched = sched;
        this.mailbox = new Mailbox();
        this.processDebuggingName = null;         
        
        /* 

        The call frames have the structure

        ---> stack growth direction --->

        |--------------------+--------------+------------------------------+---------------|
        | pc at return site  | ret callback | mclear at the time of entry  | branching bit |
        |--------------------+--------------+------------------------------+---------------|

        The branching bit indicates whether the execution of this frame invoked any branch 
        instructions. Upon returns we check whether the flag is set, and in that case 
        we enforce that the current mailbox clearance must match the one at the time of 
        the invocation.

        -- AA; 2020-02-12 
       
        */ 
       
        
        this.callStack = [ pc, null, null, BRANCH_FLAG_OFF
                         , pc, ret, this.mailbox.mclear, BRANCH_FLAG_OFF] 
                                            // auxiliary bottom element of the call stack; never called
                                            // but is convenient for keeping track of the PC 
        this._sp = this.callStack.length - 1; 
        this.next = () => {
            theFun.apply (namespace, theArgs);
        }   
        this._asserts = new Asserts (this) 
    }


    exportState ()  {
        //throw "ERROR - not implemented" // 2019-05-08 
        let state = {            
            pc  : this.pc,
            bl  : this.pc,
            pini_uuid : this.pini_uuid,
            next : this.next,
            stackdepth : this.callStack.length
            // handlerState  : this.handlerState
        }
        return state;
    }


    importState (s) {             
        this.pc = s.pc;
        this.bl = s.bl;        
        this.pini_uuid = s.pini_uuid;
        this.next = s.next;
        let n = this.callStack.length - s.stackdepth;        
        this.callStack.splice( -n ,n );
        this._sp = s.stackdepth - 1;            
    }


    addMonitor (pid, r) {        
        this.monitors[r.val] = {pid: pid, uuid: r} 
    }


    tailInThread (theFun, arg1, arg2, nm) {      
        this.next = () => {
            theFun.apply (nm, [arg1, arg2]);
        }
    }

    runNext (theFun, args, nm)  {
        this.next = () => {
            theFun.apply (nm, args);
        }
    }

    
    retStep(arg) {
        this.returnInThread(arg)
        this.sched.stepThread();             
    }


    block(cb) {
        this.next = () => {
            cb( );
        }
    }


    callInThread (cb) {
        this.callStack.push (this.pc)
        this.callStack.push ( cb ) 
        this.callStack.push ( this.mailbox.mclear )
        this.callStack.push (BRANCH_FLAG_OFF) 
        this._sp += FRAMESIZE;
    }

    setBranchFlag () {
        this.callStack[this._sp - BRANCHFLAGOFFSET] = BRANCH_FLAG_ON
    }
    
    returnInThread (arg) {       
        let rv = new LVal (arg.val
                    ,  lub  (arg.lev, this.pc)
                    ,  lub  (arg.tlev, this.pc));

        let branchFlag = this.callStack.pop ()
        let lclear = this.callStack.pop()
        let ret = this.callStack.pop ();
        this.pc = this.callStack.pop();

        if (branchFlag) {
            if (lclear != this.mailbox.mclear) {
                this.threadError (`Mailbox clearance label is not restorted after being raised in a branch; stack depth = ${this.callStack.length}` )
            }
        }
        
        this._sp -= FRAMESIZE; 
        this.next = () => {
            ret (rv);
        }
    }

    
    mkUuidVal () {
        let pid = uuidv4();
        let uuidval = this.mkVal ( pid );
        return uuidval;  
    }  


    pcpinipush ( auth: any, purpose: PCDeclassificationPurpose )  {
        let uid = uuidv4()
        let cap = this.mkVal (new Capability(uid,
                    { bl: this.bl
                    , pc: this.pc
                    , auth : auth                    
                    , purpose: purpose
                    },
                    this.pini_uuid));
                
        this.pini_uuid = uid;
        this.retStep(cap)
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
            return; // does not schedule anything in this thread 
                    // effectively terminating the thread
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
            let j = this._sp - PCOFFSET; 
            while (j >= 0 && !levels.flowsTo (this.callStack[j], pc)) {                                
                this.callStack[j] = pc;
                j -= FRAMESIZE;
            }            

            // make sure we can restore earlier caps
            this.pini_uuid = cap.prev;
            // return in the thread through the scheduler
            this.retStep (this.rtObj.__unit);                        
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
        // check that the provided authority is sufficient to perform declassification to the next level
        let ok_to_declassify = 
            levels.flowsTo (levFrom, levels.lub ( auth.val.authorityLevel, levTo ));
        if (ok_to_declassify) {
            this.bl = levTo ; 
            this.pini_uuid = cap.prev;
            this.retStep (this.rtObj.__unit);                        
        } else {
            this.threadError ( "Not enough authority for pini declassification\n" + 
                            ` | from level of the blocking level: ${levFrom.stringRep()}\n` +
                            ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n`  +
                            ` | to level of the blocking level: ${levTo.stringRep()}`);
        }        
    }


    raiseBlockingThreadLev (l) {                
        this.bl = lub (this.bl, l)        
    }

    raiseCurrentThreadPCToBlockingLev (l) {        
        this.pc = lub(this.pc, this.bl ) ;
    }

    raiseCurrentThreadPC (l)  {        
        this.pc = lub( this.pc, l )        
        this.raiseBlockingThreadLev(this.pc); 
            // 2018-11-29: AA; observe that we are raise the blocking level
            // automaticaly every time we raise the PC level.
    }

    get blockingTopLev () {
        return this.bl;         
    }

    get joinedLev () {
        return lub (this.blockingTopLev, this.pc) ;
    }

    mkVal(x) {
        return new LVal(x, this.pc, this.pc );
    }

    mkValPos(x: any, pos: string) {
        return new LVal (x, this.pc, this.pc, pos);
    }

    mkValWithLev(x:any, l:any) {            
        
        return new LVal ( x
                        , lub(this.pc, l)
                        , this.pc )              
    }

    mkCopy (x) {
        return new LVal(x.val, lub(x.lev, this.pc), lub (x.tlev, this.pc) )
    }

    
    printPc () {
        console.log ("PC:", this.pc.stringRep());
        console.log ("BL:", this.blockingTopLev.stringRep());
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
        // 2018-12-07: AA; eventually the monitoring semantics may 
        // need to be put in here      
        if ( this.handlerState.isNormal()) {  
          console.log (colors.red ( "Runtime error in thread " + this.tidErrorStringRep()))
          console.log (colors.red ( ">> " + s) );
          if (internal)  {
            throw "ImplementationError"
          }
          else {
            throw new ThreadError(s);
          }
        } else {
          console.log (colors.yellow (`Warning: runtime exception in the handler or sandbox: ${s}`))
          let f = this.handlerState.getTrapper();
          // assert and taint
          this.rtObj.assertIsFunction(f, true); //  the true flag indicates 
          // that this error should not be trapped because the exception mechanism is
          // internal to the runtime, so no assertions are normal here and therefore
          // their violation must be flagged as implementation bugs.
          this.raiseCurrentThreadPC(f.lev);
          throw "HandlerError" 
          // interrupt the execution, 
          // and pass the control to the the scheduler; that will schedule the
          // execution of the handler 
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
        let cap = this.mkVal (new Capability(uid, this.mailbox.mclear, this.mailbox.caps)) 
        this.mailbox.caps = uid;
        this.mailbox.mclear = new MboxClearance(lub (new_lclear.val, this.mailbox.mclear.boost_level), this.pc);

        this.returnInThread( cap ); 
        this.sched.stepThread();         
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
        this.returnInThread(this.rtObj.__unit)
        this.sched.stepThread();         
    }
}

