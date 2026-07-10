import { UserRuntimeZero, Constructor, mkBase, mkService } from './UserRuntimeZero.mjs'
import { assertNormalState, assertIsNTuple, assertIsLevel, assertIsList, assertIsNumber, assertIsUnit, assertIsFunction } from '../Asserts.mjs'
import { flowsTo, lub, glb, BOT } from '../Level.mjs';
import { RuntimeInterface } from '../RuntimeInterface.mjs';
import { ReceiveTaintAction } from '../ReceiveTaintAction.mjs';
import { LVal } from '../Lval.mjs';
import { mkTuple } from '../ValuesUtil.mjs';
import { __unit } from '../UnitVal.mjs';
import SandboxStatus from '../SandboxStatus.mjs';
import { Thread } from '../Thread.mjs';
import { debug } from 'console';



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
          // peek is CHECK-FREE (it removes nothing, so it carries neither the occurrence
          // nor the floor premise); its whole disclosure is confined by the read taint,
          // which carries the region folds Δ ⊔ Δlab. Under the REGION-COVERED consume
          // occurrence premise the fold terms are necessary: a secret-driven in-region
          // consume — admissible under the coverage — perturbs the residual low
          // sub-stream, and a fold-free peek would hand that perturbation to a low
          // observer before the close pays for it. Machine-checked on the Lean side
          // (ranged_receive_peek_needs_region_fold_under_covered_occurrence.lean); the
          // earlier fold-free verdict was correct only under the hard occurrence premise.
          return this.runtime.__mbox.peek (
              lub (this.runtime.$t.pc, i.lev, lowb.lev, highb.lev, highb.val,
                   mclear.boost_level, mclear.delta, mclear.deltaLab),
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

          let theThread = this.runtime.$t
          let mclear = theThread.mailbox.mclear

          // Premise 1 — occurrence, REGION-COVERED: the region is the declassification
          // boundary for ALL mailbox observations, removals included, so an in-region
          // consume's occurrence is covered by the ambient fold:
          // pc ⊔ ld(i) ⊔ ld(l1) ⊔ ld(l2) ⊑ l1 ⊔ Δ. During coexistence the coverage side
          // also carries the legacy standing clearance, exactly as the admission does —
          // this restores the shape of the original combined check
          // pc ⊔ highb ⊑ lowb ⊔ clearance, whose pc was clearance-covered all along.
          // Outside any region (Δ = ⊥, clearance = ⊥) the premise degenerates to the
          // hard pc ⊔ ld(·) ⊑ l1.
          let occ = lub (theThread.pc, i.lev, lowb.lev, highb.lev)
          let occCover = lub (lowb.val, mclear.delta, mclear.boost_level)
          if (!flowsTo (occ, occCover)) {
            let errorMessage =
              "Ranged-receive consume occurrence check failed: whether the removal fires depends on data above the region's coverage\n" +
              ` | receive lower bound (floor)        : ${lowb.val.stringRep()}\n` +
              ` | coverage (floor, region, clearance): ${occCover.stringRep()}\n` +
              ` | occurrence level (occ)             : ${occ.stringRep()}\n` +
              ` | pc level                           : ${theThread.pc.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // Premise 2 — floor: the consume may not read below any open region floor. Φ ⊑ l1.
          if (!flowsTo (mclear.phi, lowb.val)) {
            let errorMessage =
              "Ranged-receive consume floor check failed: the consume reads below an active region floor\n" +
              ` | receive lower bound (floor): ${lowb.val.stringRep()}\n` +
              ` | active floor (Phi)         : ${mclear.phi.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // Premise 3 — admission: lev(i) ⊔ l2 ⊑ l1 ⊔ Δ. The legacy standing clearance
          // (boost_level) is kept in the target alongside Δ so legacy raisembox programs
          // still admit.
          let is_admitted =
            flowsTo (lub (i.lev, highb.val), lub (lowb.val, mclear.delta, mclear.boost_level))
          if (!is_admitted) {
            let errorMessage =
              "Not enough mailbox clearance for this receive\n" +
              ` | receive lower bound: ${lowb.val.stringRep()}\n` +
              ` | receive upper bound: ${highb.val.stringRep()}\n` +
              ` | index label        : ${i.lev.stringRep()}\n` +
              ` | region fold (Delta): ${mclear.delta.stringRep()}\n` +
              ` | mailbox clearance  : ${mclear.boost_level.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // Legacy protection (retained; vacuous without raisembox, since pc_at_creation
          // stays ⊥): the pc at the time of raising the standing clearance must not itself
          // be a leak for this receive.
          if (!flowsTo (mclear.pc_at_creation, glb (theThread.pc, lowb.val))) {
            let errorMessage =
              "PC level at the time of raising the mailbox clearance is too sensitive for this receive\n" +
              ` | receive lower bound: ${lowb.val.stringRep()}\n` +
              ` | pc level at the time of receive: ${theThread.pc.stringRep()}\n` +
              ` | pc level at the time of raise: ${mclear.pc_at_creation.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // Blocking label absorbs ld(l1) ⊔ ld(l2) ⊔ lev(i) ⊔ l2 (the __mbox.consume path
          // additionally raises bl by l2 ⊔ boost_level).
          theThread.raiseBlockingThreadLev (lub (lowb.lev, highb.lev, i.lev, highb.val))

          // Result taint: v.data ⊔ pc ⊔ lev(i) ⊔ l2 ⊔ Δ ⊔ Δlab ⊔ Φlab, plus the ld(l1)/ld(l2)
          // operand terms; v.data is joined inside __mbox.consume. boost_level kept (legacy).
          let consume_l = lub (theThread.pc, i.lev, lowb.lev, highb.lev, highb.val,
                               mclear.boost_level, mclear.delta, mclear.deltaLab, mclear.phiLab)
          return this.runtime.__mbox.consume ( consume_l, i.val, lowb.val, highb.val )
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

    }
}
