import levels = require ('./options')
import { LVal } from './Lval.js';
import { ThreadError } from './ThreadError.js';
import colors = require('colors/safe');
import { StackItem } from './StackItem';
import { StackCallItem } from './StackCallItem';

let lub = levels.lub;


class Mailbox extends Array  {
    newMessage (x) {
        this.push(x);
    }
}

class StackFrame {
    pc: any;
    ret: any;
    constructor (pc, ret) {
        this.pc = pc;
        this.ret = ret;
    }
}



export class Thread {
    tid: any;
    // ret: any;
    pc: any;
    blockinglev: any;
    blockingdepth: any;
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


    constructor(tid, ret, theFun, theArgs, nm, pc, blockinglev, handlerState, rtObj) {
        this.tid = tid;
        // this.ret = ret;
        this.pc = pc;
        this.blockinglev = blockinglev;
        this.blockingdepth = blockinglev.length;

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

        if (!pc) {
            console.trace();
            throw "PC is null"
        }
        if (!blockinglev) {
            console.trace();
            throw "blocking level undefined"
        }
        if (!this.blockinglev.length) {
            // console.log ("length of blocking lev", this.blockinglev.length);
            console.trace();
            throw "Blocking level is not an array";
        }
        if (handlerState == null ) {
            console.trace();
            throw "handler state is null"
        }

        this.mailbox = new Mailbox();

        this.next = () => {
            theFun.apply (nm, theArgs);
        }        
    }

   

    get ret () {
        return this.callStack[this._sp];
    }



    exportState ()  {
        //throw "ERROR - not implemented" // 2019-05-08 
        let state = {            
            pc  : this.pc,
            blockinglev   : this.blockinglev.slice(0), // obs: important to not create an alias
            blockingdepth : this.blockingdepth,
            next : this.next,
            stackdepth : this.callStack.length
            // handlerState  : this.handlerState
        }
        return state;
    }


    importState (s) {              
        //throw "ERRROR" // 2019-05-08 
        this.pc  = s.pc;
        this.blockinglev = s.blockinglev.slice(0);        
        this.blockingdepth = s.blockingdepth;
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


    returnInThread (arg) {
        this.callStackRet(arg);
    }
  


    block(cb) {
        this.next = () => {
            cb( );
        }
    }


    callStackPush (cb) {
       this.callStack.push (this.pc)
       this.callStack.push ( cb ) 
       this._sp += 2;
    }

    callStackRet (arg) {
       
        let rv = new LVal (arg.val
                    ,  lub  (arg.lev, this.pc)
                    ,  lub  (arg.tlev, this.pc));

//        let frame = this.callStack.pop();
        let ret = this.callStack.pop ();
        this.callStack.pop();
  
        this._sp -= 2; 
       
        // -- let ret = frame.ret ;

        this.pc = this.callStack[this._sp - 1]

        this.next = () => {
            ret (rv);
        }
    }


    pinipush (l, cap) {
        this.blockingdepth ++;
        this.blockinglev.unshift ( { lev : this.blockinglev[0].lev, auth : l, cap: cap } );
    }

    pinipop (cap) {
        this.blockingdepth -- ;
        if (this.blockingdepth <= 0) {
            this.threadError ("unmatched pinipop");
        }
        let r = this.blockinglev.shift();
        return r
    }


    raiseBlockingThreadLev (l) {        
        // OBS: 2019-02-27: important that we create a new object;
        // otherwise we get into aliasing issues when saving/restoring
        // thread state in sandboxing; AA 
        this.blockinglev[0] = { lev : lub (this.blockinglev[0].lev, l), 
                                auth: this.blockinglev[0].auth , 
                                cap : this.blockinglev[0].cap
                              }
    }

    raiseCurrentThreadPCToBlockingLev (l) {        
        this.pc = lub(this.pc, this.blockinglev[0].lev ) ;
    }

    raiseCurrentThreadPC (l)  {        
        this.pc = lub( this.pc, l )
        
        this.raiseBlockingThreadLev(this.pc); 
            // 2018-11-29: AA; observe that we are raise the blocking level
            // automaticaly every time we raise the PC level.
    }

    get blockingTopLev () {
        return this.blockinglev[0].lev;         
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

