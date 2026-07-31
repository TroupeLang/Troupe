import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { Record } from '../Record.mjs';
import { lub, Level } from '../Level.mjs';
import { assertIsBoolean, assertIsLocalObject, assertIsNTuple, assertIsProcessId, assertNormalState } from '../Asserts.mjs'
import { __unitbase } from '../UnitBase.mjs';
import { mkTuple } from '../ValuesUtil.mjs';
import { RuntimeInterface } from '../RuntimeInterface.mjs';
import { __nodeManager } from '../NodeManager.mjs';
import runId from '../runId.mjs';
import { stdio_level, IFC_MODEL, checkChannelEffect, suspendReadline } from './stdio.mjs';

/**
 * Terminal primitives.
 *
 * Three pure queries over the stdio channel — whether a stream is a terminal,
 * what its dimensions are, and what level the channel runs at — and three
 * operations that change it: the terminal's line discipline (`ttyRawMode`) and
 * the arming and disarming of event delivery (`ttySubscribe`,
 * `ttyUnsubscribe`). The queries change no state: no listeners, no termios, no
 * interaction with readline, so they behave identically under both stdio
 * models, an observation being no effect and nothing for the sink check to
 * refuse. The three operations are effects on the channel and carry the sink
 * check under the IFC model, as `fwrite` does — a subscription resumes the
 * stream and starts draining input from the OS buffer, which a later reader can
 * observe.
 *
 * Labelling follows the channel model
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §1.1, §5.1):
 *
 *  - `ttyIsTTY` and `ttySize` return observations drawn *through* the channel,
 *    so their results carry the channel level, exactly as `freadln` labels a
 *    line with `mkValWithLev(s, stdio_level)` (stdio.mts).
 *  - `ttyLevel` returns run configuration — which level the operator started
 *    the runtime with — not anything the channel carried, so it returns at the
 *    caller's pc. This is the deliberate contrast with `levelOf`, which labels
 *    a returned level at `lub(pc, l)` because a value's level is provenance
 *    about that value (levelops.mts).
 *
 * Error disposition follows the SimpleFileIO rule: a bad argument is fatal, a
 * bad environment is a value. A non-descriptor or the wrong descriptor kills
 * the thread; a stream that is a pipe rather than a terminal is an `Err`
 * record, because `process.stdout.columns` and `process.stdin.setRawMode` are
 * *undefined* on a pipe and an unguarded read or call would surface as an
 * internal runtime crash. `setRawMode` throwing — EIO on a terminal that has
 * gone away — is the same class and is also an `Err`.
 *
 * Helper names carry a prefix: every builtin mixin contributes to one prototype
 * chain, so a plain `mkOk`/`mkErr` here would be shadowed by the identically
 * named helper of a mixin applied later (simplefileio.mts has both).
 */
/**
 * Whether *this* component put stdin into raw mode. A program that read a line
 * on a terminal is in readline's raw mode instead, and readline's own `close()`
 * resets that; the restore hook must not reach past its own doing.
 */
let weSetRawMode = false;

/**
 * The single subscriber, as the labelled pid `ttySubscribe` was handed. Kept
 * module-private rather than in the process registry (`whereis.mts`): the
 * registry is a user-visible name→pid table any code at ROOT can rebind, which
 * would put keystroke delivery one rebind away from being hijacked
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §2.5).
 *
 * Exactly one subscriber: raw terminal input is a consumable stream, and two
 * subscribers would each get a copy of every chunk and both try to interpret
 * partial escape sequences. Fan-out belongs in Troupe, where it can be done at
 * the right level under an explicit policy.
 */
let subscriber: LVal | null = null;

/**
 * The runtime, captured when a subscription is armed. The delivery handlers run
 * from Node callbacks, outside the builtin object and outside any thread, and
 * this is how they reach the mailbox and the scheduler.
 */
let subscriberRuntime: RuntimeInterface | null = null;

/** Whether the delivery listeners are currently attached. */
let listenersAttached = false;

/**
 * Build the labelled value of one event.
 *
 * This runs with **no current thread**: there is no `$t` to take a pc from and
 * none may be read (§2.3). Every level is therefore written explicitly, and it
 * is the channel level in every position — presence and payload alike (§2.4).
 *
 * The shape is the monitor notification's, the runtime's existing precedent for
 * a message it builds itself: a flat tuple whose first component is a plain
 * string tag (`("DONE", ref, pid, reason)`, Scheduler.mts). A `datatype`
 * constructor is not available to the runtime — a constructor value carries a
 * content hash of the declaration that produced it — so string tags are what
 * crosses the boundary, and `lib/Tty.trp` converts them.
 */
function ttyEvent(components: any[]): LVal {
    const parts = components.map((c) => new LVal(c, stdio_level));
    return parts.length === 1
        // A one-component event is a bare tagged value, not a tuple: Troupe has
        // no one-element tuple, and `("TTYEOF")` in a pattern is the string
        // "TTYEOF" in parentheses (probe: tests/_unautomated/claude/tty-delivery).
        ? parts[0]
        : new LVal(mkTuple(parts), stdio_level);
}

/**
 * Deliver one event to the subscriber, or, if the subscriber has died, tear the
 * whole subscription down.
 *
 * The mailbox ingress path is the network's, unchanged: `addMessage` from a
 * Node callback with no current thread, then `resumeLoopAsync`
 * (`receiveFromRemote`, runtimeMonitored.mts). It is safe from here because
 * `addMessage` never reads the scheduler's current thread — it uses the
 * *recipient's* creation-time pc for the metadata — and the presence level is
 * passed explicitly rather than taken from an ambient thread.
 *
 * The liveness test is the safety net behind the supervisor (§2.5): a dead
 * subscriber means nothing will ever leave raw mode, so the next event that
 * arrives unsubscribes and restores the terminal. It costs one map lookup per
 * chunk and turns "editor died, terminal wedged" into "editor died, terminal
 * restored on the next keypress".
 */
function deliver(msg: LVal) {
    if (subscriber === null || subscriberRuntime === null) {
        return;
    }
    if (!subscriberRuntime.__sched.isAlive(subscriber)) {
        ttyRestore();
        return;
    }
    subscriberRuntime.__mbox.addMessage(
        __nodeManager.getNodeId(), subscriber, msg, stdio_level);
    subscriberRuntime.__sched.resumeLoopAsync();
}

/**
 * One chunk, one message. No coalescing: a paste already arrives as one or a
 * few large chunks because the tty layer delivers what is in the buffer, and
 * holding bytes back would add latency to the single-keystroke case, the one
 * that matters for feel (§2.6).
 *
 * `latin1` decoding gives one code unit per byte, losslessly and independently
 * of where the chunk boundaries fall. `utf8` here would be wrong twice over: it
 * mangles the bytes a terminal delivers that are not valid UTF-8, and it
 * corrupts a multibyte character split across two chunks before the Troupe
 * decoder ever sees it.
 */
function onStdinData(chunk: Buffer) {
    deliver(ttyEvent(["TTYDATA", chunk.toString('latin1')]));
}

function onStdinEnd() {
    deliver(ttyEvent(["TTYEOF"]));
}

function onStdoutResize() {
    const cols = process.stdout.columns;
    const rows = process.stdout.rows;
    if (typeof cols !== 'number' || typeof rows !== 'number') {
        // A resize with no dimensions to report says nothing; the event carries
        // the new size or it is not an event.
        return;
    }
    deliver(ttyEvent(["TTYRESIZE", cols, rows]));
}

function attachTtyListeners() {
    if (listenersAttached) {
        return;
    }
    process.stdin.on('data', onStdinData);
    process.stdin.on('end', onStdinEnd);
    process.stdout.on('resize', onStdoutResize);
    // A 'data' listener normally puts the stream in flowing mode by itself, but
    // not one that an earlier `pause()` stopped: resume it explicitly so that a
    // re-subscription after `ttyUnsubscribe` delivers again.
    process.stdin.resume();
    listenersAttached = true;
}

function detachTtyListeners() {
    if (!listenersAttached) {
        return;
    }
    process.stdin.removeListener('data', onStdinData);
    process.stdin.removeListener('end', onStdinEnd);
    process.stdout.removeListener('resize', onStdoutResize);
    listenersAttached = false;
}

/**
 * Reset the terminal state the runtime is responsible for. Called from
 * `cleanupAsync` (runtimeMonitored.mts), which runs for every program, on every
 * termination route that reaches it — so this must be safe when nothing was set
 * up, and safe to run twice.
 *
 * It runs outside any thread: no labels, no `$t`, no scheduler.
 *
 * Detaching the listeners and pausing is not housekeeping: it is what releases
 * the event-loop reference an active subscription holds. Resetting the mode
 * alone would leave a program that halts with a live subscription running
 * forever (§4.2).
 *
 * It restores *termios*, not screen state. The alternate screen buffer, cursor
 * visibility and cursor shape are escapes a program emitted, and unwinding them
 * is the program's business
 * (_dev_planning/text-editor/stdin-stdout-primitive-design.md §4.4).
 */
export function ttyRestore() {
    try {
        detachTtyListeners();
    } catch (e) {
        // Nothing this hook does may fail cleanup.
    }
    subscriber = null;
    subscriberRuntime = null;
    try {
        if (weSetRawMode && process.stdin.isTTY) {
            process.stdin.setRawMode(false);
        }
    } catch (e) {
        // A terminal that has gone away throws here; cleanup must not fail.
    }
    weSetRawMode = false;
    try {
        // Releases the event-loop reference the stream may hold. Separate from
        // the reset above so that a throwing reset does not skip it.
        process.stdin.pause();
    } catch (e) {
        // Same: nothing this hook does may fail cleanup.
    }
}

export function BuiltinTty<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        /** The channel level joined with the pc: the level of an observation. */
        private ttyObservationLevel(): Level {
            return lub(this.runtime.$t.pc, stdio_level);
        }

        private mkTtyOk(value: LVal): LVal {
            const rec = Record.mkRecord([
                ['tag', new LVal('Ok', this.ttyObservationLevel())],
                ['value', value],
            ]);
            return this.runtime.$t.mkValWithLev(rec, stdio_level);
        }

        private mkTtyErr(reason: string): LVal {
            const lev = this.ttyObservationLevel();
            const errRec = Record.mkRecord([
                ['reason', new LVal(reason, lev)],
            ]);
            const rec = Record.mkRecord([
                ['tag', new LVal('Err', lev)],
                ['error', new LVal(errRec, lev)],
            ]);
            return this.runtime.$t.mkValWithLev(rec, stdio_level);
        }

        /**
         * Type-assert the argument as a descriptor and check it names one of
         * the admissible streams. The wrong descriptor is a programming error
         * and kills the thread, as it does for `freadln`/`fwrite`.
         */
        private ttyDescriptor(arg, admissible: any[], expected: string) {
            assertIsLocalObject(arg);
            const fd = arg.val._value;
            if (!admissible.includes(fd)) {
                this.runtime.$t
                    .threadError(`value ${arg.val.stringRep()} is not ${expected}`);
            }
            return fd;
        }

        ttyIsTTY = mkBase((arg) => {
            assertNormalState("ttyIsTTY")
            const fd = this.ttyDescriptor(
                arg, [process.stdin, process.stdout, process.stderr],
                "a standard descriptor");
            return this.runtime.ret(
                this.runtime.$t.mkValWithLev(fd.isTTY === true, stdio_level));
        }, "ttyIsTTY");

        ttySize = mkBase((arg) => {
            assertNormalState("ttySize")
            const fd = this.ttyDescriptor(
                arg, [process.stdout, process.stderr],
                "an output descriptor");

            if (fd.isTTY !== true) {
                return this.runtime.ret(this.mkTtyErr("not a terminal"));
            }
            const cols = fd.columns;
            const rows = fd.rows;
            if (typeof cols !== 'number' || typeof rows !== 'number') {
                return this.runtime.ret(this.mkTtyErr("terminal size unavailable"));
            }
            const lev = this.ttyObservationLevel();
            const size = Record.mkRecord([
                ['cols', new LVal(cols, lev)],
                ['rows', new LVal(rows, lev)],
            ]);
            return this.runtime.ret(this.mkTtyOk(new LVal(size, lev)));
        }, "ttySize");

        ttyLevel = mkBase((arg) => {
            assertNormalState("ttyLevel")
            this.ttyDescriptor(
                arg, [process.stdin, process.stdout, process.stderr],
                "a standard descriptor");
            // Run configuration, not channel content: the result is at the pc.
            return this.runtime.ret(this.runtime.$t.mkVal(stdio_level));
        }, "ttyLevel");

        /**
         * Put the terminal into or out of raw mode.
         *
         * Raw mode changes the echo and the line discipline of the channel
         * visibly, so under the IFC model it carries the same sink check as
         * `fwrite`; under the capability model it is unchecked, admission
         * having been decided when the descriptor was acquired.
         *
         * Entering raw mode first releases readline: on a terminal the
         * interface owns stdin's mode and its own echo, so raw mode has to be
         * set from a base state readline is not holding. A later `freadln`
         * re-creates the interface.
         */
        ttyRawMode = mkBase((arg) => {
            assertNormalState("ttyRawMode")
            assertIsNTuple(arg, 2);
            const fd = this.ttyDescriptor(
                arg.val[0], [process.stdin], "an input descriptor");
            assertIsBoolean(arg.val[1]);
            const on = arg.val[1].val === true;

            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t, "terminal raw-mode change",
                                   arg.lev, arg.val[0].lev, arg.val[1].lev)
            }

            if (fd.isTTY !== true || typeof fd.setRawMode !== 'function') {
                return this.runtime.ret(this.mkTtyErr("not a terminal"));
            }
            try {
                if (on) {
                    suspendReadline();
                    fd.setRawMode(true);
                    weSetRawMode = true;
                } else {
                    fd.setRawMode(false);
                    weSetRawMode = false;
                }
            } catch (e) {
                return this.runtime.ret(
                    this.mkTtyErr(e instanceof Error ? e.message : String(e)));
            }
            return this.runtime.ret(
                this.mkTtyOk(new LVal(__unitbase, this.ttyObservationLevel())));
        }, "ttyRawMode");

        /**
         * Direct terminal events to a process.
         *
         * Arming delivery is an effect on the channel — the stream resumes and
         * input starts draining from the OS buffer, which a later `freadln` can
         * observe — so it carries the sink check under the IFC model. It
         * additionally raises the caller's pc by the argument levels, as
         * `spawn` does with the closure it spawns (spawn.mts): which process
         * receives keystrokes is a decision, and a decision must be visible in
         * the pc.
         *
         * The pid must be local. Delivery to a remote pid would vanish without
         * a word — `addMessage` drops a message to a pid it cannot find alive
         * (MailboxProcessor.mts) — so a non-local pid is a programming error
         * and kills the thread, in the same class as the wrong descriptor.
         *
         * A TTY is deliberately not required: on a pipe the same listeners
         * deliver the same messages, which is what makes the whole delivery
         * path testable without a pseudo-terminal (§1.3).
         */
        ttySubscribe = mkBase((arg) => {
            assertNormalState("ttySubscribe")
            assertIsNTuple(arg, 2);
            const fd = this.ttyDescriptor(
                arg.val[0], [process.stdin], "an input descriptor");
            assertIsProcessId(arg.val[1]);

            this.runtime.$t.raiseCurrentThreadPC(
                lub(arg.lev, arg.val[0].lev, arg.val[1].lev));

            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t, "terminal subscription",
                                   arg.lev, arg.val[0].lev, arg.val[1].lev)
            }

            const pid = arg.val[1].val;
            if (pid.uuid == null || pid.uuid.toString() !== runId.toString()) {
                this.runtime.$t
                    .threadError(`process ${arg.val[1].val.stringRep()} is not local; ` +
                                 `terminal events can only be delivered to a local process`);
            }

            // Re-subscribing replaces the pid; the listeners stay as they are.
            subscriber = arg.val[1];
            subscriberRuntime = this.runtime;
            attachTtyListeners();

            return this.runtime.ret(
                this.mkTtyOk(new LVal(__unitbase, this.ttyObservationLevel())));
        }, "ttySubscribe");

        /**
         * Stop terminal event delivery.
         *
         * Idempotent: `Ok` whether or not anything was subscribed, so that a
         * supervisor can call it unconditionally on its teardown path (§1.3).
         * Pausing the stream is what releases the event-loop reference the
         * subscription held, and is therefore what lets a program that has
         * unsubscribed still exit on its own (§3.4).
         */
        ttyUnsubscribe = mkBase((arg) => {
            assertNormalState("ttyUnsubscribe")
            this.ttyDescriptor(arg, [process.stdin], "an input descriptor");

            if (IFC_MODEL) {
                checkChannelEffect(this.runtime.$t, "terminal unsubscription",
                                   arg.lev)
            }

            detachTtyListeners();
            subscriber = null;
            subscriberRuntime = null;
            try {
                process.stdin.pause();
            } catch (e) {
                // A stream that has gone away is already as paused as it gets.
            }

            return this.runtime.ret(
                this.mkTtyOk(new LVal(__unitbase, this.ttyObservationLevel())));
        }, "ttyUnsubscribe");
    }
}
