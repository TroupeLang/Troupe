import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { lub, ROOT } from '../Level.mjs';
import {
    assertIsAuthority, assertIsNTuple, assertIsProcessId,
    assertIsRootAuthority, assertNormalState
} from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { RuntimeInterface } from '../RuntimeInterface.mjs';
import { __nodeManager } from '../NodeManager.mjs';
import runId from '../runId.mjs';

/**
 * Signal disposition.
 *
 * `trapSigterm (authority, pid)` directs SIGTERM to a local process instead of
 * to the operating system's default disposition; `untrapSigterm authority`
 * gives it back. With a trap in place the runtime delivers a message and does
 * nothing else — the program decides when and how to stop, and stops itself
 * with `exit`. Without one the process dies on the signal exactly as it always
 * has.
 *
 * The listener exists only while a trap does. That is a measured constraint,
 * not a style choice: a JS listener replaces the OS disposition for the whole
 * process, so the signal is dispatched on the event loop, which the scheduler
 * reaches only when `loop` returns — after up to 500,000 x 1,000 CPS steps
 * (Scheduler.mts). Measured on tests/rt/timeout/diverging/loop2.trp, an
 * always-installed listener deferred a `kill -TERM` by 207 seconds and turned
 * `gtimeout 8` into 195-215 seconds, which would leave every shell-`timeout`
 * harness in the repository bounding nothing. A program that traps has chosen
 * that latency for itself; a program that does not is left alone.
 *
 * Both require ROOT authority (`assertIsRootAuthority`, the check `exit`
 * performs — exit.mts). Trapping SIGTERM is the power to make the node ignore
 * the operator's kill request, a process-lifecycle capability in the same
 * family as `exit`, `register` and `persist`, not a data channel: if
 * terminating the node requires ROOT, deferring its termination does too, and
 * delivered mobile code holding quarantined or attenuated authority cannot
 * register itself and swallow kills.
 *
 * This is not an exception to the stdio channel model
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §1.1, §4.3).
 * stdio moved to checks-at-the-operations because a descriptor names a
 * labelled channel; signal disposition is global runtime state, which is where
 * operations require ROOT authority.
 *
 * Registration mechanics follow `ttySubscribe` (tty.mts): one subscriber,
 * module-private, replaced on re-registration, a non-local pid a thread error,
 * and a pc raise by the argument levels as `spawn` performs.
 *
 * Scope is SIGTERM alone. SIGINT stays with `bulletProofSigint`, and in raw
 * mode Ctrl-C arrives as the byte 0x03 with no signal raised at all.
 */

/**
 * The single trapping process, as the labelled pid `trapSigterm` was handed.
 * Module-private for the same reason the terminal subscriber is: the process
 * registry (whereis.mts) is a user-visible name→pid table any code at ROOT can
 * rebind, which would put signal delivery one rebind away from being hijacked.
 */
let sigtermSubscriber: LVal | null = null;

/**
 * The runtime, captured when a trap is registered. The signal handler runs from
 * a Node callback, outside any thread, and this is how it reaches the mailbox
 * and the scheduler.
 */
let sigtermRuntime: RuntimeInterface | null = null;

/**
 * Whether a registered trap is still able to receive. A registration whose
 * process has died is not a trap: the same liveness rule the terminal
 * subscriber follows (§2.5), and the reason a dead trap-holder cannot wedge a
 * node against `kill`.
 */
function hasLiveSigtermTrap(): boolean {
    if (sigtermSubscriber === null || sigtermRuntime === null) {
        return false;
    }
    return sigtermRuntime.__sched.isAlive(sigtermSubscriber);
}

/**
 * Deliver SIGTERM to the trapping process, or, with no live trap, give the
 * signal back to the operating system.
 *
 * The mailbox ingress path is the network's, unchanged: `addMessage` from a
 * Node callback with no current thread, then `resumeLoopAsync`
 * (`receiveFromRemote`, runtimeMonitored.mts). It is safe from here because
 * `addMessage` never reads the scheduler's current thread — it uses the
 * *recipient's* creation-time pc for the metadata — and the presence level is
 * passed explicitly rather than taken from an ambient thread. No `$t` is read
 * anywhere on this path.
 *
 * The wire shape is the bare string "SIGTERM": a one-component event travels
 * untupled, as "TTYEOF" does. Presence and payload are both ROOT — a kill is a
 * root-integrity operator action, independent of `--stdiolev`, so the level is
 * fixed rather than the stdio channel's.
 *
 * Repeated SIGTERMs deliver one message each: a second kill request is a
 * distinct request, and swallowing it would leave an operator who escalates
 * from one `kill` to another with nothing to observe.
 *
 * A registration whose process has died is not a trap. Detaching the listener
 * and re-raising restores the default disposition — Node stops listening for a
 * signal once its last listener is removed — so the process then dies on the
 * signal exactly as an untrapped one does, and a dead trap-holder cannot wedge
 * a node against `kill`.
 */
function onSigterm() {
    if (!hasLiveSigtermTrap()) {
        detachSigtermListener();
        process.kill(process.pid, 'SIGTERM');
        return;
    }
    const msg = new LVal("SIGTERM", ROOT);
    sigtermRuntime.__mbox.addMessage(
        __nodeManager.getNodeId(), sigtermSubscriber, msg, ROOT);
    sigtermRuntime.__sched.resumeLoopAsync();
}

/** Whether the listener is attached. Trapping twice must not attach twice. */
let listenerAttached = false;

function attachSigtermListener() {
    if (listenerAttached) {
        return;
    }
    process.on('SIGTERM', onSigterm);
    listenerAttached = true;
}

function detachSigtermListener() {
    if (!listenerAttached) {
        return;
    }
    process.removeListener('SIGTERM', onSigterm);
    listenerAttached = false;
}

export function BuiltinSignals<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        /**
         * Direct SIGTERM to a process.
         *
         * The pid must be local. Delivery to a remote pid would vanish without
         * a word — `addMessage` drops a message to a pid it cannot find alive
         * (MailboxProcessor.mts) — and a node whose kill signal goes nowhere is
         * exactly what the liveness rule exists to prevent, so a non-local pid
         * is a programming error and kills the thread.
         *
         * The pc rises by the argument levels, as `spawn` does with the closure
         * it spawns: which process receives the signal is a decision, and a
         * decision must be visible in the pc.
         *
         * Returns unit. There is no environment-dependent failure here — no
         * terminal to be absent, no descriptor to be wrong — so the
         * `Ok`/`Err` record `ttySubscribe` returns would carry no information,
         * and both signal builtins return unit alike.
         */
        trapSigterm = mkBase((arg) => {
            assertNormalState("trapSigterm")
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0]);
            assertIsProcessId(arg.val[1]);
            assertIsRootAuthority(arg.val[0]);

            this.runtime.$t.raiseCurrentThreadPC(
                lub(arg.lev, arg.val[0].lev, arg.val[1].lev));

            const pid = arg.val[1].val;
            if (pid.uuid == null || pid.uuid.toString() !== runId.toString()) {
                this.runtime.$t
                    .threadError(`process ${arg.val[1].val.stringRep()} is not local; ` +
                                 `SIGTERM can only be delivered to a local process`);
            }

            // Re-registering replaces the pid; there is one trap at a time.
            sigtermSubscriber = arg.val[1];
            sigtermRuntime = this.runtime;
            attachSigtermListener();

            return this.runtime.ret(__unit);
        }, "trapSigterm");

        /**
         * Give SIGTERM back to the operating system's default disposition.
         *
         * Idempotent: unit whether or not anything was trapped, so that a
         * supervisor can call it unconditionally on its teardown path, the same
         * property `ttyUnsubscribe` has.
         */
        untrapSigterm = mkBase((arg) => {
            assertNormalState("untrapSigterm")
            assertIsAuthority(arg);
            assertIsRootAuthority(arg);

            detachSigtermListener();
            sigtermSubscriber = null;
            sigtermRuntime = null;

            return this.runtime.ret(__unit);
        }, "untrapSigterm");
    }
}
