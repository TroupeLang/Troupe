import { UserRuntimeZero, Constructor, mkBase, mkService } from './UserRuntimeZero.mjs'
import { assertNormalState, assertIsNTuple, assertIsLevel, assertIsList, assertIsNumber, assertIsUnit, assertIsFunction, assertIsAuthority } from '../Asserts.mjs'
import { flowsTo, lub, glb, BOT, privFlowsTo } from '../Level.mjs';
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
          let theThread = this.runtime.$t
          let mclear = theThread.mailbox.mclear
          // peek is CHECK-FREE (it removes nothing, so it carries neither the occurrence
          // nor the floor premise); its whole disclosure is confined by the read taint,
          // which carries the active ceiling and its label, Δ ⊔ Δlab. Under the consume
          // occurrence premise's receive clearance the ceiling terms are necessary: a
          // secret-driven in-region consume — within the receive clearance — perturbs the
          // residual low sub-stream, and a peek without the ceiling terms would hand that
          // perturbation to a low observer before the close pays for it. Machine-checked
          // on the Lean side
          // (ranged_receive_peek_needs_region_fold_under_covered_occurrence.lean);
          // only under the hard occurrence premise would the ceiling terms be redundant.
          //
          // The same receive-clearance reasoning governs the peek's BLOCKING channel: whether an
          // i-th in-interval message exists is a firing decision that reads the operands
          // and the sub-stream up to the active ceiling (which in-region removals perturb
          // secret-dependently), so the operand labels and the ceiling terms quarantine
          // the thread's blocking label: bl ⊔= ld(i) ⊔ ld(l1) ⊔ ld(l2) ⊔ Δ ⊔ Δlab.
          // (The __mbox.peek path additionally raises bl by l2 ⊔ boost_level.)
          theThread.raiseBlockingThreadLev (lub (i.lev, lowb.lev, highb.lev,
                                                 mclear.delta, mclear.deltaLab))
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

          // The occurrence premise: the region is the declassification boundary for
          // ALL mailbox observations, removals included, so an in-region consume's
          // occurrence must flow to the receive clearance — the read's floor joined
          // with the active ceiling:
          // pc ⊔ ld(i) ⊔ ld(l1) ⊔ ld(l2) ⊑ l1 ⊔ Δ. During coexistence the receive
          // clearance also carries the legacy standing clearance, exactly as the
          // selection premise's bound does — this restores the shape of the original
          // combined check pc ⊔ highb ⊑ lowb ⊔ clearance, whose pc the standing
          // clearance cleared all along.
          // Outside any region (Δ = ⊥, clearance = ⊥) the premise degenerates to the
          // hard pc ⊔ ld(·) ⊑ l1.
          let occ = lub (theThread.pc, i.lev, lowb.lev, highb.lev)
          let rcvClearance = lub (lowb.val, mclear.delta, mclear.boost_level)
          if (!flowsTo (occ, rcvClearance)) {
            let errorMessage =
              "Ranged-receive consume occurrence check failed: whether the removal fires depends on data above the receive clearance\n" +
              ` | receive lower bound (floor): ${lowb.val.stringRep()}\n` +
              ` | receive clearance          : ${rcvClearance.stringRep()}\n` +
              ` | occurrence level (occ)     : ${occ.stringRep()}\n` +
              ` | pc level                   : ${theThread.pc.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // The floor premise: the consume may not read below any open region floor. Φ ⊑ l1.
          if (!flowsTo (mclear.phi, lowb.val)) {
            let errorMessage =
              "Ranged-receive consume floor check failed: the consume reads below the active floor\n" +
              ` | receive lower bound (floor): ${lowb.val.stringRep()}\n` +
              ` | active floor (Phi)         : ${mclear.phi.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // The selection premise: lev(i) ⊔ l2 ⊑ l1 ⊔ Δ. The legacy standing clearance
          // (boost_level) is kept in the target alongside Δ so legacy raisembox programs
          // still pass.
          let selection_ok =
            flowsTo (lub (i.lev, highb.val), lub (lowb.val, mclear.delta, mclear.boost_level))
          if (!selection_ok) {
            let errorMessage =
              "Not enough mailbox clearance for this receive\n" +
              ` | receive lower bound   : ${lowb.val.stringRep()}\n` +
              ` | receive upper bound   : ${highb.val.stringRep()}\n` +
              ` | index label           : ${i.lev.stringRep()}\n` +
              ` | active ceiling (Delta): ${mclear.delta.stringRep()}\n` +
              ` | mailbox clearance     : ${mclear.boost_level.stringRep()}`
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

          // Blocking label absorbs ld(l1) ⊔ ld(l2) ⊔ lev(i) ⊔ l2 ⊔ Δ ⊔ Δlab ⊔ Φlab — the
          // operand labels plus the active-range registers the firing decision reads: the
          // occurrence and the selection premises draw on the sub-stream up to the active
          // ceiling, and the floor check reads Φ under Φlab, so the active-range terms
          // quarantine the consume's blocking channel exactly as the read taint confines
          // its value channel. (The __mbox.consume path additionally raises bl by
          // l2 ⊔ boost_level.)
          theThread.raiseBlockingThreadLev (lub (lowb.lev, highb.lev, i.lev, highb.val,
                                                 mclear.delta, mclear.deltaLab, mclear.phiLab))

          // Result taint: v.data ⊔ pc ⊔ lev(i) ⊔ l2 ⊔ Δ ⊔ Δlab ⊔ Φlab, plus the ld(l1)/ld(l2)
          // operand terms; v.data is joined inside __mbox.consume. boost_level kept (legacy).
          let consume_l = lub (theThread.pc, i.lev, lowb.lev, highb.lev, highb.val,
                               mclear.boost_level, mclear.delta, mclear.deltaLab, mclear.phiLab)
          return this.runtime.__mbox.consume ( consume_l, i.val, lowb.val, highb.val )
        })

        // The native instant authorized consume — consumeWithAuthority (i, lowb, highb, auth).
        // A direct runnable oracle for the formal machine's authority-carrying consume rule:
        // remove the i-th message with presence in [lowb, highb], authorized per selection by
        // the SHOWN authority (no standing clearance region, no raise/lower protocol). Under
        // the ranged-receive v2 protocol it is exactly the region-free (Delta = BOT) instance
        // of the encoded bracket
        //   enableRangedReceive (lowb, lev(i) ⊔ highb, auth); consume; disableRangedReceive
        // (floored_bracket_iff), which is why the regionlessness premise below is a premise
        // and not a courtesy: the formal machine's point mailbox observations require no open
        // region, and the equivalence is stated at the top level.
        consumeWithAuthority = mkBase (arg => {
          assertIsNTuple(arg, 4)
          assertIsNumber(arg.val[0])
          assertIsLevel (arg.val[1])
          assertIsLevel (arg.val[2])
          assertIsAuthority (arg.val[3])
          let i = arg.val[0]
          let lowb = arg.val[1]
          let highb = arg.val[2]
          let auth = arg.val[3]

          let theThread = this.runtime.$t
          let mclear = theThread.mailbox.mclear

          // The regionlessness premise: the instant form is the bare-thread (top-level)
          // instance. An open clearance region — a non-empty capability chain, from
          // enableRangedReceive or the legacy raisembox — is refused; so is a residual
          // legacy standing clearance (boost_level above BOT). Inside a region the
          // program already holds the bracket spelling; the instant form does not
          // compose with a standing region's active range.
          if (theThread.mailbox.caps != null) {
            let errorMessage =
              "consumeWithAuthority requires no open clearance region: an enableRangedReceive (or legacy raisembox) region is open\n" +
              ` | open capability chain head: ${theThread.mailbox.caps}`
            theThread.threadError (errorMessage)
          }
          if (!flowsTo (mclear.boost_level, BOT)) {
            let errorMessage =
              "consumeWithAuthority requires no standing mailbox clearance\n" +
              ` | mailbox clearance: ${mclear.boost_level.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // The occurrence premise, HARD (no active ceiling, no boost): whether the
          // removal fires must not depend on data above the floor. The authority
          // operand's .lev joins in — the selection check reads the authority, so it
          // co-determines whether the removal fires.
          // pc ⊔ i.lev ⊔ lowb.lev ⊔ highb.lev ⊔ auth.lev ⊑ lowb.
          let occ = lub (theThread.pc, i.lev, lowb.lev, highb.lev, auth.lev)
          if (!flowsTo (occ, lowb.val)) {
            let errorMessage =
              "consumeWithAuthority occurrence check failed: whether the removal fires depends on data above the floor\n" +
              ` | receive lower bound (floor): ${lowb.val.stringRep()}\n` +
              ` | occurrence level (occ)     : ${occ.stringRep()}\n` +
              ` | pc level                   : ${theThread.pc.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // The selection premise, authorized by the SHOWN authority's level alone (the
          // standing clearance plays no role): privFlowsTo aLev (highb ⊔ lev(i)) lowb —
          // on integrity, I_auth ∧ I_(highb ⊔ lev(i)) ⟹ I_lowb.
          let Hi = lub (highb.val, i.lev)
          let aLev = auth.val.authorityLevel
          if (!privFlowsTo (aLev, Hi, lowb.val)) {
            let errorMessage =
              "Insufficient authority for this consume: the shown authority does not cover the selection down to the floor\n" +
              ` | receive lower bound (floor)            : ${lowb.val.stringRep()}\n` +
              ` | receive upper bound                    : ${highb.val.stringRep()}\n` +
              ` | index label                            : ${i.lev.stringRep()}\n` +
              ` | selection boundary (highb ⊔ lev(i))    : ${Hi.stringRep()}\n` +
              ` | authority provided                     : ${aLev.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // Blocking label absorbs the operand data labels plus the selection boundary:
          // whether an i-th in-interval message exists is a firing decision that reads all
          // four operands (the authority operand included — the selection check consults
          // it, mirroring the enable's auth.lev join). bl ⊔= i.lev ⊔ lowb.lev ⊔ highb.lev
          // ⊔ auth.lev ⊔ highb ⊔ i.lev. (The __mbox.consume path additionally raises bl
          // by highb ⊔ boost_level; boost_level = BOT here by the regionlessness premise.)
          theThread.raiseBlockingThreadLev (lub (i.lev, lowb.lev, highb.lev, auth.lev, Hi))

          // Result: readTainted at occ ⊔ highb ⊔ i.lev — v.data is joined inside
          // __mbox.consume. No active-range/boost terms: the regionlessness premise pins
          // them at BOT, so this is exactly the formal readTainted level. Unlike the three-argument consume this
          // builtin writes nothing back into any standing clearance: the instant rule
          // raises bl and taints the result but grows no persistent mailbox taint.
          let consume_l = lub (occ, Hi)
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
