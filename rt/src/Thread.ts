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


class Mailbox extends Array  {
    newMessage (x) {
        this.push(x);
    }
}

// class StackFrame {
//     pc: any;
//     ret: any;
//     constructor (pc, ret) {
//         this.pc = pc;
//         this.ret = ret;
//     }
// }



export class Thread {
    tid: any;    
    pc: any;
    bl: any;
    pinistack: any;
    pinidepth: any;
    handlerState: any;
    monitors: {};
    killCounter: number;
    sleeping: boolean;
    timeoutObject: any;
    rtObj: any;
    mailbox: Mailbox;

    next :  () => void;

    callStack : any []


    _sp : number;

    


    constructor(tid, ret, theFun, theArgs, namespace, pc, levblock, handlerState, rtObj) {
        this.tid = tid;    
        this.pc = pc;
        this.bl = levblock;
        this.pinistack = [];
        this.pinidepth = 0;
        this.handlerState = handlerState;
        this.monitors = {};
        this.killCounter = 0;
        this.sleeping = false;
        this.timeoutObject = null;
        this.rtObj = rtObj;
        this._sp = 3;
        this.callStack = [ pc, null
                         , pc, ret ]   // auxiliary bottom element of the call stack; never called
                                       // but is convenient for keeping track of the PC 
        this.mailbox = new Mailbox();
        this.next = () => {
            theFun.apply (namespace, theArgs);
        }        

        // if (!pc) {
        //     console.trace();
        //     throw "PC is null"
        // }
        // if (!levblock) {
        //     console.trace();
        //     throw "blocking level undefined"
        // }
        // if (!this.blockinglev.length) {
        //     // console.log ("length of blocking lev", this.blockinglev.length);
        //     console.trace();
        //     throw "Blocking level is not an array";
        // }
        // if (handlerState == null ) {
        //     console.trace();
        //     throw "handler state is null"
        // }

    }

   


    exportState ()  {
        //throw "ERROR - not implemented" // 2019-05-08 
        let state = {            
            pc  : this.pc,
            bl  : this.pc,
            pinistack   : this.pinistack.slice(0), // obs: important to not create an alias
            pinidepth : this.pinidepth,
            next : this.next,
            stackdepth : this.callStack.length
            // handlerState  : this.handlerState
        }
        return state;
    }


    importState (s) {              
        //throw "ERRROR" // 2019-05-08 
        this.pc = s.pc;
        this.bl = s.bl;
        this.pinistack = s.pinistack.slice(0);        
        this.pinidepth = s.pinidepth;
        this.next = s.next;
        let n = this.callStack.length - s.stackdepth;        
        this.callStack.splice( -n ,n );
        this._sp = s.stackdepth - 1;
        
        // this.handlerState  = s.handlerState
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

    
    retstep(arg) {
        this.rtObj.ret(arg);        
    }


    block(cb) {
        this.next = () => {
            cb( );
        }
    }


    callInThread (cb) {
        this.callStack.push (this.pc)
        this.callStack.push ( cb ) 
        this._sp += 2;
    }

    returnInThread (arg) {       
        let rv = new LVal (arg.val
                    ,  lub  (arg.lev, this.pc)
                    ,  lub  (arg.tlev, this.pc));

        let ret = this.callStack.pop ();
        this.pc = this.callStack.pop();

        this._sp -= 2; 
        
        this.next = () => {
            ret (rv);
        }
    }

    
    pcpush (l, cap) {
        this.raiseBlockingThreadLev(l.lev);        
        this.pinidepth ++;       
        this.pinistack.unshift ( { lev : this.bl, pc: this.pc, auth : l, cap: cap, purpose: 1 } );
    }


    pcpop (arg) {
        if (this.pinidepth <= 0) {
            this.threadError ("unmatched pcpop");
        }
        
        this.pinidepth -- ;
        let r = this.pinistack.shift();            
        let {lev, pc, auth, cap, purpose} = r;        
        // check the scopes
        if (arg.val != cap.val || purpose != 1) {
            this.threadError ("Ill-scoped pinipush/pinipop");
            return; // does not schedule anything in this thread 
                    // effectively terminating the thread
        }

        
        // We declassify the current blocking level to the old blocking level. 
        // and also the current pc to the old pc. 
        // We check that there is sufficient authority to go from the current blocking level
        // all the way down to the target pc 
        let levFrom = this.bl;
        let levTo = pc

        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // check that the provided authority is sufficient for the declassification
        let ok_to_declassify = 
            levels.flowsTo (levFrom, levels.lub ( auth.val.authorityLevel, levTo ));
        if (ok_to_declassify) {        
            this.pc = pc;           
            this.bl = lev; 
            // declassify the call stack...             
            let j = this._sp - 1; 
            while (j >= 0 && !levels.flowsTo (this.callStack[j], pc)) {                                
                this.callStack[j] = pc;
                j -= 2;
            }            
            this.retstep (this.rtObj.__unit);                        
        } else {
            this.threadError ( "Not enough authority for pini declassification\n" + 
                            ` | from level of the blocking level: ${levFrom.stringRep()}\n` +
                            ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n`  +
                            ` | to level of the blocking level: ${levTo.stringRep()}`);
        }                
    }
    


    pinipush (auth, cap) {
        this.raiseBlockingThreadLev(auth.lev);        
        this.pinidepth ++;       
        let obj = { lev : this.bl, pc: this.pc, auth : auth, cap: cap, purpose: 0 };
        this.pinistack.unshift ( obj );
    }


    pinipop (arg) {
        if (this.pinidepth <= 0) {
            this.threadError ("unmatched pinipop");
        }

        this.pinidepth -- ;        
        let r = this.pinistack.shift();            
        this.raiseBlockingThreadLev(this.pc); // maintaining the invariant that the blocking level is as high as the pc level       
        let {lev, auth, cap, purpose} = r;
        // check the scopes

        if (arg.val != cap.val || purpose != 0) {            
            this.threadError ("Ill-scoped pinipush/pinipop");
            return; // does not schedule anything in this thread 
                    // effectively terminating the thread
        }

        // If we are here then the pinipop is well-scoped
        // so we check the declassifications now

        let levFrom = this.bl;
        let levTo = lev;

        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // check that the provided authority is sufficient to perform declassification to the next level
        let ok_to_declassify = 
            levels.flowsTo (levFrom, levels.lub ( auth.val.authorityLevel, levTo ));
        if (ok_to_declassify) {
            this.bl = levTo ; 
            this.retstep (this.rtObj.__unit);                        
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

    mkValWithLev(x, l) {            
        
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

    threadError (s, internal = false) {
        // 2018-12-07: AA; eventually the monitoring semantics may 
        // need to be put in here      
        if ( this.handlerState.isNormal()) {  
          console.log (colors.red ( "Runtime error in thread " + this.tid.stringRep()))
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
}

