import { Thread, Capability } from './Thread';

const __unitbase = require('./UnitBase.js');
const { isListFlagSet, isTupleFlagSet } = require ('./ValuesUtil.js');
const proc = require('./process.js');
const ProcessID = proc.ProcessID;
import { Level } from './Level';
import { Authority } from './Authority'
import options = require('./options.js');
import { TroupeType } from './TroupeTypes';
const levels = options;
const flowsTo = levels.flowsTo;


export class Asserts {
    _thread : Thread 

    constructor (t: Thread) {        
        this._thread = t
    }

    assertIsNumber (x:any) {
        this._thread.raiseBlockingThreadLev(x.tlev)        
        if ( typeof x.val != 'number') {
            this._thread.threadError ("value " + x.stringRep() + " is not a number") 
        }
    }

    assertIsBoolean (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);  
        if ( typeof x.val != 'boolean') {
            this._thread.threadError ("value " + x.stringRep() + " is not a boolean")
        }
    }

    assertIsFunction(x:any, internal=false) {  
        this._thread.raiseBlockingThreadLev(x.tlev);  
        if (x.val._troupeType != TroupeType.CLOSURE) {
            this._thread.threadError ("value " + x.stringRep() + " is not a function", internal)
        }
    }
    
    assertIsHandler(x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);  
        if (x.val._troupeType != TroupeType.CLOSURE) {
            this._thread.threadError ("value " + x.stringRep() + " is not a handler")
        } 
    }

    assertIsUnit (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if (!x.val._is_unit) {
            this._thread.threadError ( "value " + x.stringRep () + " is not unit")
        }
    }


    assertIsListOrTuple (x:any) {
        this._thread.raiseBlockingThreadLev(x.tlev);;
        if ( !((isListFlagSet (x.val) || isTupleFlagSet(x.val) ) ) ) {
            this._thread.threadError ("value " + x.stringRep() + " is not a list or tuple" )
        }
    }

    assertIsList (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);;
        if ( !isListFlagSet (x.val)) {
            this._thread.threadError ("value " + x.stringRep() + " is not a list" )
        }
    }

    assertIsNTuple (x: any, n: number) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if (!(Array.isArray (x.val) && isTupleFlagSet (x.val)  && x.val.length == n )) {
            this._thread.threadError ("value " + x.stringRep() + " is not a " + n + "-tuple" )
        }
    }



    assertIsString (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if ( typeof x.val != 'string') {
            this._thread.threadError ("value " + x.stringRep() + " is not a string")
        }
    }


    assertIsNode (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if ( typeof x.val != 'string') {
            this._thread.threadError ("value " + x.stringRep() + " is not a node string") // todo: check for it being a proper nodeid format?
        } 
        if (x.val.startsWith ("@")) {
            if (!this._thread.rtObj.__nodeManager.aliases[x.val.substring(1)]) {
                this._thread.threadError (`${x.val} is not a defined alias`)
            }
        }
    }

    assertIsProcessId (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if (! (x.val instanceof ProcessID)) {
            this._thread.threadError ("value " + x.stringRep() + " is not a process id")
        }
    }


    assertIsCapability (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev); 
        if (! (x.val instanceof Capability)) {
            this._thread.threadError ("value " + x.stringRep() + " is not a capability of lowering the mailbox clearance")
        }
    }

    assertIsLevel (x:any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if (! (x.val instanceof Level)) {
            this._thread.threadError ("value " + x.stringRep() + " is not a level");
        }
    }

    assertIsTopAuthority (x: any) {
        let isTop = flowsTo (levels.TOP, x.val.authorityLevel);
        if (!isTop) {
            let errorMessage = 
                "Provided authority is not TOP\n" +
                ` | level of the provided authority: ${x.val.authorityLevel.stringRep()}`
            this._thread.threadError (errorMessage);
        }
    }

    assertIsAuthority (x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);
        if (! (x.val instanceof Authority )) {
            this._thread.threadError ("value " + x.stringRep() + " is not a authority");
        } 
    }

    assertIsEnv(x: any) {
        this._thread.raiseBlockingThreadLev(x.tlev);    
        if (! (x.val._is_rt_env)) {
            this._thread.threadError ("value " + x.stringRep() + " is not an environment");
        }  
    }

    assertNormalState (s: string) {
        if (!this._thread.handlerState.isNormal()) {
            this._thread.threadError ("invalid handler state in " + s + " -- side effects are prohbited in handler pattern matching or sandboxed code")
        }
    }

    assertDeclassificationAllowed(s: string) {
        if (!this._thread.handlerState.declassificationAllowed ()) {
            this._thread.threadError ("invalid handler state in " + s + ": declassification prohibited in handler pattern matching")
        }
    }
    


    assertPairAreNumbers(x:any,y:any) {
        this.assertIsNumber (x);
        this.assertIsNumber (y);
    }

    assertPairAreStringsOrNumbers (x:any,y:any) {
        this._thread.raiseBlockingThreadLev(x.tlev);    
        switch (typeof x.val) {
            case 'number' : this.assertIsNumber(y); break;
            case 'string' : this.assertIsString(y); break;
            default: this._thread.threadError ("values " + x.stringRep () + " and " + y.stringRep() + " are of different types")
        }
    }


}
