import { UserRuntimeZero, Constructor, mkBase, mkService } from './UserRuntimeZero'
import { assertNormalState, assertIsNTuple, assertIsLevel, assertIsList, assertIsAtom, assertIsNumber, assertIsUnit, assertIsFunction } from '../Asserts';
import { flowsTo, lub, glb, BOT } from '../options';
import { RuntimeInterface } from '../RuntimeInterface';
import { ReceiveTaintAction } from '../ReceiveTaintAction';
import { LVal } from '../Lval';
import { mkTuple } from '../ValuesUtil';
import { __unit } from '../UnitVal';
import SandboxStatus from '../SandboxStatus';
import { Thread } from '../Thread';



/*
// this function must only be called from 
// one of the checked functions 
function _receiveFromMailbox ($r:RuntimeInterface, lowb, highb, handlers) {
  let mclear = $r.$t.mailbox.mclear
  
  let is_sufficient_clearance = 
    flowsTo( lub (highb.val, $r.$t.pc)
          ,  lub (lowb.val, mclear.boost_level ))

    if (!is_sufficient_clearance)  {  
      let errorMessage = 
        "Not enough mailbox clearance for this receive\n" +
        ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
        ` | receive upper bound: ${highb.val.stringRep()}\n` +
        ` | pc level           : ${$r.$t.pc.stringRep()}\n` +
        ` | mailbox clearance  : ${mclear.boost_level.stringRep()}` 
      $r.$t.threadError (errorMessage);
    }    
  
    let is_clearance_a_leak = flowsTo( mclear.pc_at_creation, glb ($r.$t.pc, lowb.val))

    if (!is_clearance_a_leak)  {
      let errorMessage = 
        "PC level at the time of raising the mailbox clearance is too sensitive for this receive\n" +
        ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
        ` | pc level at the time of receive: ${$r.$t.pc.stringRep()}\n` +        
        ` | pc level at the time of raise: ${mclear.pc_at_creation.stringRep()}`  // we need better terminology for these       
      $r.$t.threadError (errorMessage);
    }


    return $r.__mbox.rcv(lowb.val, highb.val, handlers, mclear.boost_level)
    
}
*/

 
/** Receiving functionality; 2020-02-12; AA 
 *
 * Observe that we have three receive functions. 
 *
 * 1. The most general one is called `rcv` and it takes a 3-tuple of the form
 *    (low_bound_lev, high_bound_lev handlers), and performs an
 *    interval receive on all messages from the lower to the higher bound.
 *    Because this sort of ranged modifies the state of the mailbox in a way
 *    that leaks information, it is necessary that the mailbox has sufficient
 *    clearance. The implementation of this function checks that the
 *    clearance is sufficient; this check is perfomed similaly to how
 *    declassification checks are performed. 
 *
 * 2. Receive on a point interval, `rcvp`. A sugar for (1)
 *
 * 3. Receive on a point consisting of the current program counter, `receive`.
 *    We include this option only for backward compatibility with many earlier
 *    examples.
 *
 *
 */ 




export function BuiltinReceive<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        peek = mkBase (arg => {
          assertIsNTuple(arg, 3)
          assertIsNumber(arg.val[0])
          assertIsLevel (arg.val[1])
          assertIsLevel (arg.val[2])
          let i = arg.val[0]
          let lowb = arg.val[1]
          let highb = arg.val[2]
          let mclear = this.runtime.$t.mailbox.mclear
          return this.runtime.__mbox.peek (
              lub (this.runtime.$t.pc, i.lev, lowb.lev, highb.lev, highb.val, mclear.boost_level), 
              i.val, lowb.val, highb.val )
        })

        consume = mkBase (arg => {
          assertIsNTuple(arg, 3)
          assertIsNumber(arg.val[0])
          assertIsLevel (arg.val[1])
          assertIsLevel (arg.val[2])
          let i = arg.val[0]
          let lowb = arg.val[1]
          let highb = arg.val[2]

          let $r = this.runtime
          let mclear = $r.$t.mailbox.mclear
          let is_sufficient_clearance = 
            flowsTo( lub (highb.val, $r.$t.pc)
                  ,  lub (lowb.val, mclear.boost_level ))
      
          if (!is_sufficient_clearance)  {  
            let errorMessage = 
              "Not enough mailbox clearance for this receive\n" +
              ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
              ` | receive upper bound: ${highb.val.stringRep()}\n` +
              ` | pc level           : ${$r.$t.pc.stringRep()}\n` +
              ` | mailbox clearance  : ${mclear.boost_level.stringRep()}` 
            $r.$t.threadError (errorMessage);
          }    
        
          let is_clearance_a_leak = flowsTo( mclear.pc_at_creation, glb ($r.$t.pc, lowb.val))
      
          if (!is_clearance_a_leak)  {
            let errorMessage = 
              "PC level at the time of raising the mailbox clearance is too sensitive for this receive\n" +
              ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
              ` | pc level at the time of receive: ${$r.$t.pc.stringRep()}\n` +        
              ` | pc level at the time of raise: ${mclear.pc_at_creation.stringRep()}`  // we need better terminology for these       
            $r.$t.threadError (errorMessage);
          }
      

          return this.runtime.__mbox.consume (
              lub (this.runtime.$t.pc, i.lev, lowb.lev, highb.lev, highb.val, mclear.boost_level), 
              i.val, lowb.val, highb.val )
        })

        _blockThread = mkBase ((arg) => {
          assertIsUnit(arg)
          this.runtime.__sched.blockThread(this.runtime.__sched.__currentThread);
          return null;
        })

        _pc = mkBase ((arg) => {
          assertIsUnit (arg)
          return this.runtime.ret (
            new LVal (this.runtime.$t.pc, this.runtime.$t.pc, BOT))
        })
 

        guard = mkBase (arg => {
          assertIsNTuple(arg, 3)
          let f = arg.val[0]
          let taintLimitArg = arg.val[1]
          let def = arg.val[2]
          assertIsFunction(f)
          assertIsLevel(taintLimitArg)
          let theThread = this.runtime.$t
          theThread.raiseCurrentThreadPC(lub (f.lev, taintLimitArg.lev, taintLimitArg.val))

          let tntLim = theThread.bl 
          let guard_sp : number = null 
          theThread.handlerState = new SandboxStatus.INHANDLER (
            () => { // trapper - invoked upon side-effects and guard check failure
              theThread._sp = guard_sp 
              theThread.invalidateSparseBit()
              theThread.pc = taintLimitArg.val 
              theThread.bl = taintLimitArg.val 
              theThread.handlerState = new SandboxStatus.NORMAL ()
              return theThread.returnImmediateLValue(def)
            },
            theThread.pc,
            () => { // guard checker -- called by the scheduler on context switches  
              if (!flowsTo(theThread.bl, tntLim)) {
                theThread.threadError ("guard violation")
              }
            }
          )

          let guardFrame : any = () => {
            let arg = theThread.arg_as_lval
            let l_guard = lub (arg.lev, theThread.bl) 
            if (flowsTo(l_guard, tntLim)) {
              theThread.invalidateSparseBit()
              theThread.handlerState = new SandboxStatus.NORMAL ()
              return theThread.returnImmediate()
            } else {
              theThread.threadError ("guard violation")
            }
          }

          guardFrame.debugname = "<guardReturnFrame>"
          theThread.pushFrame ( guardFrame)
          guard_sp = theThread._sp 
          return theThread.tailCall (f.val, __unit)

        })

        receive = mkService ( () => { 
                assertNormalState("receive"); 
                return this.runtime.$service.receive()
            }, "receive")
        rcvp = mkService ( () => {
          assertNormalState ("rcvp")
          return this.runtime.$service.rcvp()
        }, "rcvp")

        rcv = mkService ( () => { 
          assertNormalState ("rcv")
          return this.runtime.$service.rcv()
        }, "rcv")

        /*
        rcv = mkBase((arg) => {
            assertNormalState("rcv");
            assertIsNTuple(arg, 3);
            assertIsLevel(arg.val[0])
            assertIsLevel(arg.val[1])
            assertIsList(arg.val[2])
            let lowb = arg.val[0]
            let highb = arg.val[1]
            let handlers = arg.val[2]
            return _receiveFromMailbox(this.runtime, lowb, highb, handlers);
        })
        */
        /*
        rcvp = mkBase((arg) => {
            assertNormalState("rcvp")
            assertIsNTuple(arg, 2)
            assertIsLevel(arg.val[0])
            assertIsList(arg.val[1])
            let lev = arg.val[0]
            let handlers = arg.val[1];
            return _receiveFromMailbox(this.runtime, lev, lev, handlers)
        })
        */

        /*
        receive = mkBase((handlers) => {
            assertNormalState("receive")
            assertIsList(handlers)
            // shortcutting level checks because they are guaranteed 
            // to hold when both low and upper bound is pc; 2020-02-08; AA

            let l = new LVal (this.runtime.$t.pc, this.runtime.$t.pc, BOT )
 
            return this.runtime.__mbox.rcv(this.runtime.$t.pc,
                     this.runtime.$t.pc, handlers,
                     this.runtime.$t.mailbox.mclear.boost_level);
        })
        */


        /* 
        rcvlim = mkBase((arg) => {
            assertNormalState("receive")
            assertIsNTuple(arg, 3)
            assertIsLevel(arg.val[0])
            assertIsAtom (arg.val[1])
            assertIsList (arg.val[2])

            
            let taintAction;
            
            switch (arg.val[1].val.atom) {
              case "KEEP": taintAction = ReceiveTaintAction.KEEP; break;
              case "DROP": taintAction = ReceiveTaintAction.DROP; break;
              default : this.runtime.$t.threadError("Invalid taint action argument")
            }
            let handlers = arg.val[2]
            this.runtime.$t.raiseCurrentThreadPC(arg.val[0].lev)
            let taintLimit = arg.val[0].val 

            assertIsList(handlers)
            // shortcutting level checks because they are guaranteed 
            // to hold when both low and upper bound is pc; 2020-02-08; AA

            return this.runtime.__mbox.rcv(this.runtime.$t.pc,
                      this.runtime.$t.pc, handlers,
                      this.runtime.$t.mailbox.mclear.boost_level,
                      taintLimit, taintAction
                      );
        })
        */
    }
}
