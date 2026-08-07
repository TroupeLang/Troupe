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
          assertNormalState("peek")
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
          // (The __mbox.peek path additionally raises bl by l2.)
          theThread.raiseBlockingThreadLev (lub (i.lev, lowb.lev, highb.lev,
                                                 mclear.delta, mclear.deltaLab))
          return this.runtime.__mbox.peek (
              lub (this.runtime.$t.pc, i.lev, lowb.lev, highb.lev, highb.val,
                   mclear.delta, mclear.deltaLab),
              i.val, lowb.val, highb.val )
        })

        consume = mkBase (arg => {
          assertNormalState("consume")
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
          // pc ⊔ ld(i) ⊔ ld(l1) ⊔ ld(l2) ⊑ l1 ⊔ Δ.
          // Outside any region (Δ = ⊥) the premise degenerates to the hard
          // pc ⊔ ld(·) ⊑ l1.
          let occ = lub (theThread.pc, i.lev, lowb.lev, highb.lev)
          let rcvClearance = lub (lowb.val, mclear.delta)
          if (!flowsTo (occ, rcvClearance)) {
            let errorMessage =
              "The data that causes this receive or consume does not flow to the level it can read.\n" +
              ` | level of the data that causes the receive or consume : ${occ.stringRep()}\n` +
              ` | level the receive or consume can read                : ${rcvClearance.stringRep()}\n` +
              ` | lower bound of the receive or consume                : ${lowb.val.stringRep()}\n` +
              ` | pc level                                             : ${theThread.pc.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // The floor premise: the consume cannot read below any open region floor. Φ ⊑ l1.
          if (!flowsTo (mclear.phi, lowb.val)) {
            let errorMessage =
              "The lower bound of the open enableRangedReceives must flow to the lower bound of this receive or consume.\n" +
              ` | lower bound of the open enableRangedReceives : ${mclear.phi.stringRep()}\n` +
              ` | lower bound of the receive or consume        : ${lowb.val.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // The selection premise: lev(i) ⊔ l2 ⊑ l1 ⊔ Δ — the same receive clearance
          // the occurrence premise bounds against.
          let selection_ok = flowsTo (lub (i.lev, highb.val), rcvClearance)
          if (!selection_ok) {
            let errorMessage =
              "The upper bound of this receive or consume does not flow to the level it can read.\n" +
              ` | upper bound of the receive or consume : ${highb.val.stringRep()}\n` +
              ` | level the receive or consume can read : ${rcvClearance.stringRep()}\n` +
              ` | level of the message index            : ${i.lev.stringRep()}`
            theThread.threadError (errorMessage);
          }

          // Blocking label absorbs ld(l1) ⊔ ld(l2) ⊔ lev(i) ⊔ l2 ⊔ Δ ⊔ Δlab ⊔ Φlab — the
          // operand labels plus the active-range registers the firing decision reads: the
          // occurrence and the selection premises draw on the sub-stream up to the active
          // ceiling, and the floor check reads Φ under Φlab, so the active-range terms
          // quarantine the consume's blocking channel exactly as the read taint confines
          // its value channel. (The __mbox.consume path additionally raises bl by
          // l2.)
          theThread.raiseBlockingThreadLev (lub (lowb.lev, highb.lev, i.lev, highb.val,
                                                 mclear.delta, mclear.deltaLab, mclear.phiLab))

          // Result taint: v.data ⊔ pc ⊔ lev(i) ⊔ l2 ⊔ Δ ⊔ Δlab ⊔ Φlab, plus the ld(l1)/ld(l2)
          // operand terms; v.data is joined inside __mbox.consume.
          let consume_l = lub (theThread.pc, i.lev, lowb.lev, highb.lev, highb.val,
                               mclear.delta, mclear.deltaLab, mclear.phiLab)
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
          assertNormalState("consumeWithAuthority")
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
          // instance, so an open region — a non-empty capability chain — is refused.
          // Inside a region the program already holds the bracket spelling; the instant
          // form does not compose with a region's active range.
          if (theThread.mailbox.caps != null) {
            let errorMessage =
              "consumeWithAuthority is not possible while an enableRangedReceive is open.\n" +
              ` | capability of the last enableRangedReceive : ${theThread.mailbox.caps}`
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
              "The data that causes this consumeWithAuthority does not flow to the level it can read.\n" +
              ` | level of the data that causes the consumeWithAuthority : ${occ.stringRep()}\n` +
              ` | level the consumeWithAuthority can read                : ${lowb.val.stringRep()}\n` +
              ` | pc level                                               : ${theThread.pc.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // The selection premise, authorized by the SHOWN authority's level alone (the
          // standing clearance plays no role): privFlowsTo aLev (highb ⊔ lev(i)) lowb —
          // on integrity, I_auth ∧ I_(highb ⊔ lev(i)) ⟹ I_lowb.
          let Hi = lub (highb.val, i.lev)
          let aLev = auth.val.authorityLevel
          if (!privFlowsTo (aLev, Hi, lowb.val)) {
            let errorMessage =
              "consumeWithAuthority cannot release the message to its lower bound with this authority.\n" +
              ` | level the selection is made at            : ${Hi.stringRep()}\n` +
              ` | lower bound given to consumeWithAuthority : ${lowb.val.stringRep()}\n` +
              ` | level of the authority                    : ${aLev.stringRep()}\n` +
              ` | upper bound given to consumeWithAuthority : ${highb.val.stringRep()}\n` +
              ` | level of the message index                : ${i.lev.stringRep()}`
            theThread.threadError (errorMessage)
          }

          // Blocking label absorbs the operand data labels plus the selection boundary:
          // whether an i-th in-interval message exists is a firing decision that reads all
          // four operands (the authority operand included — the selection check consults
          // it, mirroring the enable's auth.lev join). bl ⊔= i.lev ⊔ lowb.lev ⊔ highb.lev
          // ⊔ auth.lev ⊔ highb ⊔ i.lev. (The __mbox.consume path additionally raises bl
          // by highb.)
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
          let pcInGuard = theThread.pc
          let guard_sp : number = null
          theThread.handlerState = new SandboxStatus.INHANDLER (
            () => { // trapper - invoked upon side-effects and guard check failure
              theThread._sp = guard_sp
              theThread.invalidateSparseBit()
              theThread.pc = pcInGuard
              theThread.bl = tntLim
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
