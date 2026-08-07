import * as levels from './Level.mjs'
import { DowngradeDimension, DowngradeKind, DowngradeResult, DowngradeErrorReason, ValidateDowngradeParams } from './DowngradeEnums.mjs';
import { LVal, LValCopyAt } from './Lval.mjs';
import { HandlerError, ImplementationError, StrThreadError, ErrorKind } from './TroupeError.mjs';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import {
    getDowngradeErrorMessage,
} from './DowngradeFormatter.mjs';

const argv = getCliArgs();

let logLevel = argv[TroupeCliArg.Debug]? 'debug' : 'info'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('thread',  logLevel);
const debug = x => logger.debug(x)
let lub = levels.lub;
let flowsTo = levels.flowsTo
import { v4 as uuidv4} from 'uuid'
import Table from 'cli-table3'

import { TroupeType } from './TroupeTypes.mjs'
import { RuntimeInterface } from './RuntimeInterface.mjs';
import { __unit } from './UnitVal.mjs';
import { Level } from './Level.mjs';
import { mkTuple } from './ValuesUtil.mjs';
import { SchedulerInterface } from './SchedulerInterface.mjs';
import { getRuntimeObject } from './SysState.mjs';
import { HnState } from './SandboxStatus.mjs';


let isPiniMode = argv[TroupeCliArg.Pini]?true:false;
let isNmifcMode = argv[TroupeCliArg.Nmifc]?true:false;


export enum PCDowngradePurpose {
    Full="pcpush", 
    Pini="pinipush"
}

// stack frame configuration constants
export const CALLSIZE = 5
const SPOFFSET = CALLSIZE
const PCOFFSET  = 4
export const RETOFFSET = 3 
const MCLEAROFFSET = 2
const BRANCHFLAGOFFSET = 1

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
   
    _troupeType: TroupeType
    uid : string 
    data : T
    prev: string
    dataLevel: Level
    constructor (c: string, l:T,p: string , dataLevel ) {
        this.uid = c; 
        this.data = l;
        this.prev = p // provides scoping control; needs better name; AA; 2020-02-08
        this.dataLevel = dataLevel
        this._troupeType = TroupeType.CAPABILITY
    }
    stringRep() : string {
        return this.uid;
    }
}

/** The ACTIVE RECEIVE RANGE of the open ranged receives. This record lives inside the
 *  branch-balance discipline (returnImmediate) which snapshots and compares it by object
 *  identity, so every mutation allocates a fresh record. The active range spans ALL open
 *  regions; under nesting it is the JOIN of the open ranges (floors joined, ceilings
 *  joined), not their intersection. A region whose open reported ok = false never closes:
 *  its own disable fails on validity, and the LIFO chain-head check blocks every enclosing
 *  close while it stays open. Δ = delta, the active ceiling, is what the receive clearance
 *  draws on; Φ = phi, the active floor, is what every consume must respect; every reader of
 *  delta is tainted with deltaLab (the ceiling label), every reader of phi with phiLab (the
 *  floor label).
 */
class  MboxClearance {
  delta: any;      // the active ceiling: join of the open regions' ceilings hi (base BOT)
  phi: any;        // the active floor:   join of the open regions' floors  lo  (base BOT)
  deltaLab: any;   // the ceiling label:  join of the open regions' ld(hi)      (base BOT)
  phiLab: any;     // the floor label:    join of the open regions' ld(lo)      (base BOT)

  constructor (folds:any = null) {
    const B = levels.BOT;
    this.delta     = folds?.delta     ?? B;
    this.phi       = folds?.phi       ?? B;
    this.deltaLab  = folds?.deltaLab  ?? B;
    this.phiLab    = folds?.phiLab    ?? B;
  }

  // Allocate a fresh record, preserving every field unless overridden. `overrides`
  // carries a `folds` object with any subset of the four active-range fields.
  copyWith (overrides:any) {
    return new MboxClearance ({
      delta: this.delta, phi: this.phi, deltaLab: this.deltaLab, phiLab: this.phiLab,
      ...(overrides.folds ?? {})
    });
  }

  stringRep () {
    return "[" + this.phi.stringRep () + ".." + this.delta.stringRep () + "]"
  }
}

// The payload a ranged-receive capability carries: the enable-time clearance record
// (restored by object identity at the matching disable, so a balanced enable/disable
// inside a branch leaves the mailbox record identical and passes the branch-balance
// check), the pc at the enable and the region's floor lo (the disable's two occurrence
// floors), and the validity bit (an uncertified region's capability is invalid: no
// disable accepts it, so the region never closes).
class RangedReceiveCap {
  mclearSnapshot: MboxClearance;
  pc_enable: any;
  lo: any;
  valid: boolean;
  // The two ends of the move the disable has to make, as the enable's certification
  // saw them (each joined with the ambient ceiling), the authority shown against
  // them, and — when the certification refused — why. The disable reports the
  // refusal, so it needs the operands the enable weighed; without them it can only
  // say that something was wrong.
  levFrom: any;
  levTo: any;
  authLevel: any;
  dgReason: DowngradeErrorReason | null;
  constructor (snapshot: MboxClearance, pc_enable:any, lo:any, valid:boolean,
               levFrom:any, levTo:any, authLevel:any, dgReason: DowngradeErrorReason | null) {
    this.mclearSnapshot = snapshot;
    this.pc_enable = pc_enable;
    this.lo = lo;
    this.valid = valid;
    this.levFrom = levFrom;
    this.levTo = levTo;
    this.authLevel = authLevel;
    this.dgReason = dgReason;
  }
}


/** The mailbox is an array of messages in arrival order, scanned and
 *  indexed by physical position, plus a head offset marking the first
 *  live message. Consuming at the head — the common FIFO case — advances
 *  the offset in O(1) instead of splicing (Array.prototype.splice shifts
 *  every remaining element, which makes draining a deep mailbox
 *  quadratic). A FifoQueue cannot be used here because selective receive
 *  needs positional scanning and mid-queue removal.
 */
class Mailbox extends Array {
    mclear : MboxClearance ;
    caps : string;

    /** Physical index of the first live message; slots below it are
     *  cleared and reclaimed by compaction in consumeAt. */
    head : number;

    /** Consumed-prefix length at which compaction is considered. */
    static readonly COMPACTION_THRESHOLD = 32;

    peek_cache_index : number
    peek_cache_position: number
    peek_cache_lowb  : Level
    peek_cache_highb : Level


    constructor () {
        super ()
        this.mclear = new MboxClearance ();
        this.caps = null;
        this.head = 0;

        this.peek_cache_index = null;
        this.peek_cache_position = null;
        this.peek_cache_lowb  = null;
        this.peek_cache_highb = null
    }
    newMessage (x) {
        this.push(x);
    }

    /** Number of live (unconsumed) messages. */
    get logicalSize () {
        return this.length - this.head;
    }

    /** Remove the message at physical index i. O(1) when i is the head
     *  (the FIFO case); falls back to splice for out-of-order selective
     *  consumption. Callers must reset the peek cache first: compaction
     *  shifts physical positions. */
    consumeAt (i : number) {
        if (i === this.head) {
            this[i] = null;
            this.head++;
            if (this.head >= Mailbox.COMPACTION_THRESHOLD
                && this.head * 2 >= this.length) {
                this.splice(0, this.head);
                this.head = 0;
            }
        } else {
            this.splice(i, 1);
        }
    }


    resetPeekCache ()  {
        this.peek_cache_index = null;
        this.peek_cache_lowb  = null; 
        this.peek_cache_position = null;    
        this.peek_cache_lowb  = null; 
        this.peek_cache_highb = null
    }

}

class ThreadState {
  callStack: any [] 
  constructor (s: any []) {
     this.callStack = s
  }
}


export class SleepTimeout {    
    sleepingUntil  : number 
    timeoutObject  : any;
    resumeThread   : Thread
    timeLeft       : number;
    constructor(delay,  th) {        
        this.timeoutObject = setTimeout( () => {this.afterTimeout()}, delay);
        this.sleepingUntil = Date.now () + delay 
        this.resumeThread = th
        this.timeLeft = null;
    }

    pause () {
        clearTimeout(this.timeoutObject)
        this.timeLeft = Math.max (0, this.sleepingUntil - Date.now())
    }

    resume (newThread) { 
        this.resumeThread = newThread;
        this.sleepingUntil = Date.now() + this.timeLeft;
        this.timeoutObject = setTimeout (() => {this.afterTimeout()}, this.timeLeft)
        this.timeLeft = null;
    }

    resetTimeout(newDelay,newThread) {
        this.resumeThread = newThread;
        this.timeoutObject = setTimeout (() => {this.afterTimeout()}, newDelay)
        this.sleepingUntil = Date.now() + newDelay
    }

    afterTimeout () {
        let theThread = this.resumeThread;
        theThread.sleepTimeout = null; 
        theThread.returnSuspended (__unit);
        let sched = getRuntimeObject().__sched
        sched.scheduleThread (theThread)
        sched.resumeLoopAsync()
    }    
}

export class Thread {
    tid: any;
    pc: Level;
    bl: Level;

    // NMIFC mode flag - read from CLI args
    get isNmifcMode(): boolean {
        return isNmifcMode;
    }


    // registers 
    r0_val: any;
    private _r0_lev: any;
    public get r0_lev(): any {
        return this._r0_lev;
    }
    public set r0_lev(value: any) {
        if (!value?.isLevel ) {
            debug ("RO-LEV debugging")
            debug (new Error().stack)
            if (argv[TroupeCliArg.Debug]) {
                this.showStack()
            }
        }
        this._r0_lev = value;
    }
    r0_tlev: any;

    pini_uuid : string;

    handlerState: HnState;
    monitors: {};
    killCounter: number;
    // sleeping: boolean;
    // sleepingUntil: number;
    // wakeupFn: () => void;
    // timeoutObject: any;
    sleepTimeout : SleepTimeout
    rtObj: RuntimeInterface;
    sched : SchedulerInterface;
    mailbox: Mailbox;
    next :  () => any;
    callStack : any []
    _sp : number;
    sparseSlot : number; // slot on the stack holding the sparse bit (whether data is bounded by PC)

    processDebuggingName: string;

    // Source position of the last tail call, used for error reporting when
    // errors occur inside runtime built-ins (where user code isn't on the JS stack)
    lastCallSourcePos: string | null = null;

    /**
     * Current source map for the executing code.
     * Set by function/continuation preambles in generated code.
     * Used by error handlers to translate JS positions to Troupe positions.
     * This is a V3 source map object (version, sources, mappings, etc.)
     */
    currentSourceMap: any | null = null;

    failureRate: number  = 0
    failureStartTime : number = 0

    get sleeping () {
        return (this.sleepTimeout != null);
    } 
    
    constructor(tid, ret, theFun, arg, pc, levblock, handlerState, rtObj, sched) {
        this.tid = tid;    
        this.pc = pc;
        this.bl = levblock;
        this.pini_uuid = null;
        this.handlerState = handlerState;
        this.monitors = {};
        this.killCounter = 0;
        this.sleepTimeout = null; // no sleep command        
        // this.sleeping = false;
        // this.timeoutObject = null;
        this.rtObj = rtObj;
        this.sched = sched;
        this.mailbox = new Mailbox();
        this.processDebuggingName = null;         
        
        /* 

        The call frames have the structure



        Stack growth direction: downward (increasing indices)

        +-----------------------------+-------------------+
        | sp - 5                      | sp_prev           |
        +-----------------------------+-------------------+
        | sp - 4                      | pc at return site |
        +-----------------------------+-------------------+
        | sp - 3                      | ret callback      |
        +-----------------------------+-------------------+
        | sp - 2                      | mclear at entry   |
        +-----------------------------+-------------------+
        | sp - 1                      | branching bit     |
        +-----------------------------+-------------------+
        | sp ... (sp + framesize)     | [escaping locals] |
        +-----------------------------+-------------------+
        | sp + framesize + 1          | sparse slot       |
        +-----------------------------+-------------------+

        
        The branching bit indicates whether the execution of this frame invoked any branch 
        instructions. Upon returns we check whether the flag is set, and in that case 
        we enforce that the current mailbox clearance must match the one at the time of 
        the invocation.

        -- AA; 2020-02-12 

        +-------------------+
        | prev_sp           |
        +-------------------+
        | pc_at_ret_point   |
        +-------------------+
        | ret_cb            |
        +-------------------+
        | mclear            |
        +-------------------+
        | branch_bit        |
        +-------------------+
        | <... locals ...>  |  <-- sp
        +-------------------+
       
        */ 
       
        
        /*                                                                
        this.callStack = [ 0, pc, null, null, BRANCH_FLAG_OFF
                         , 5, pc, ret, this.mailbox.mclear, BRANCH_FLAG_OFF] 
                                            // auxiliary bottom element of the call stack; never called
                                            // but is convenient for keeping track of the PC 
        this._sp = CALLSIZE * 2
        */

        this.callStack = [ 0, pc, ret, this.mailbox.mclear, BRANCH_FLAG_OFF]                              
        this._sp = CALLSIZE 
        // let arg0 = theArgs [0]
        // let arg1 = theArgs [1]
        this.r0_val = arg.val 
        this.r0_lev = arg.lev 
        this.r0_tlev = arg.tlev 
        this.next = theFun
        // () => {            
        //     return theFun();
        // }           
    }


    exportState ()  {
        let __state = {            
            pc             : this.pc,
            bl             : this.pc,
            pini_uuid      : this.pini_uuid,
            sp             : this._sp,
            next           : this.next,            
            callStack      : this.callStack,
            r0_val         : this.r0_val,
            r0_lev         : this.r0_lev,
            r0_tlev        : this.r0_tlev  
            
        }
        return __state;
    }

    importState (__state) {             
        this.pc =        __state.pc
        this.bl =        __state.bl  
        this.pini_uuid = __state.pini_uuid
        this._sp =       __state.sp
        this.next =      __state.next
        this.callStack = __state.callStack
        this.r0_val =    __state.r0_val          
        this.r0_lev =    __state.r0_lev         
        this.r0_tlev =   __state.r0_tlev 
    }

    resetStackForSandboxing () {
      this.callStack = []
      this._sp = 0
    }


    // Diagnostic stack dump; writes to stderr so it does not corrupt
    // program stdout.
    showStack ()  {
        console.error ("======== SHOW STACK ========= ")
        console.error (`sp = ${this._sp} sparseSlot = ${this.sparseSlot}`)
        let j = this._sp - 1
        let stack = this.callStack
        while ( j > 0) {
            console.error (`-${j.toString().padStart(5,'-')} branch bit: ${stack[j--]}`)
            let mclear = stack[j]
            console.error (` ${j.toString().padStart(5,' ')} mclear    : ${mclear?.stringRep()}`)
            j --
            let ret = stack [j]
            let ret_string = ret?.debugname
            if (!ret_string) {
                ret_string = ret?.toString ()
            }

            console.error (` ${j.toString().padStart(5,' ')} ret       : ${ret_string}`)
            j --
            console.error (` ${j.toString().padStart(5,' ')} pc_ret    : ${stack[j]?.stringRep()}`)
            j --
            console.error (` ${j.toString().padStart(5,' ')} sp_prev   : ${stack[j]}`)
            console.error (` ${(j-1).toString().padStart(5,' ')} sparse    : ${stack[j-1]}`)
            let sp_prev = stack[j];
            j = sp_prev - 1 ;
        }
    }

    showStackV2(options: { maxDepth?: number, showLocals?: boolean } = {}): string {
        const { maxDepth = Infinity, showLocals = false } = options;

        const boxChars = {
            'top': '═', 'top-mid': '╤', 'top-left': '╔', 'top-right': '╗',
            'bottom': '═', 'bottom-mid': '╧', 'bottom-left': '╚', 'bottom-right': '╝',
            'left': '║', 'left-mid': '╟', 'mid': '─', 'mid-mid': '┼',
            'right': '║', 'right-mid': '╢', 'middle': '│'
        };

        const lines: string[] = [];

        // Metadata table - current thread state
        const metaTable = new Table({
            chars: boxChars,
            style: { head: [], border: [] },
            colWidths: [20, 58]
        });

        const truncate = (s: string, len: number) => s.length > len ? s.substring(0, len - 3) + '...' : s;

        const tidStr = this.tidErrorStringRep();
        const pcStr = this.pc?.stringRep?.() ?? 'undefined';
        const blStr = this.bl?.stringRep?.() ?? 'undefined';
        const r0LevStr = this.r0_lev?.stringRep?.() ?? 'undefined';
        const r0TlevStr = this.r0_tlev?.stringRep?.() ?? 'undefined';
        const r0ValStr = truncate(String(this.r0_val), 55);
        const sparseVal = this.sparseSlot != null ? String(this.callStack[this.sparseSlot]) : 'N/A';

        metaTable.push(
            [{ colSpan: 2, content: 'STACK TRACE', hAlign: 'center' }],
            ['Thread ID', truncate(tidStr, 55)],
            ['Process Name', this.processDebuggingName ?? '(not set)'],
            ['Current PC', truncate(pcStr, 55)],
            ['Blocking Level', truncate(blStr, 55)],
            ['SP / Sparse Slot', `${this._sp} / ${this.sparseSlot ?? 'N/A'} (value: ${sparseVal})`],
            ['R0 val', r0ValStr],
            ['R0 lev', truncate(r0LevStr, 55)],
            ['R0 tlev', truncate(r0TlevStr, 55)],
            ['Pini UUID', this.pini_uuid ?? '(null)']
        );

        lines.push('');
        lines.push(metaTable.toString());

        // Count total frames first
        // Frame layout: sp_prev is at position (j - 4) when j points to branch_bit (sp - 1)
        let totalFrames = 0;
        let countJ = this._sp - 1;
        while (countJ > 0) {
            totalFrames++;
            // j points to branch_bit; sp_prev is 4 positions before (SPOFFSET - BRANCHFLAGOFFSET = 5 - 1 = 4)
            const spPrev = this.callStack[countJ - (SPOFFSET - BRANCHFLAGOFFSET)];
            countJ = spPrev - 1;
        }

        if (totalFrames === 0) {
            const emptyTable = new Table({
                chars: boxChars,
                style: { head: [], border: [] },
                colWidths: [78]
            });
            emptyTable.push([{ content: '(stack is empty)', hAlign: 'center' }]);
            lines.push(emptyTable.toString());
            const output = lines.join('\n');
            console.log(output);
            return output;
        }

        // Frames - plain text format, no borders
        lines.push(`STACK FRAMES (${totalFrames} total)`);
        lines.push('─'.repeat(78));

        let j = this._sp - 1;
        let stack = this.callStack;
        let frameNum = 0;
        let prevFrameSp = this._sp;

        while (j > 0 && frameNum < maxDepth) {
            // Branch bit (at sp - 1)
            const branchBitIdx = j;
            const branchBit = stack[j--];
            const branchStr = branchBit ? 'ON (raised)' : 'OFF';

            // Mclear (at sp - 2)
            const mclearIdx = j;
            const mclear = stack[j--];
            const mclearStr = truncate(mclear?.stringRep?.() ?? 'null', 57);

            // Return callback (at sp - 3)
            const retIdx = j;
            const ret = stack[j--];
            let retString = ret?.debugname ?? ret?.name;
            if (!retString) {
                const retToStr = ret?.toString?.() ?? 'null';
                retString = truncate(retToStr, 57);
            }

            // PC at return (at sp - 4)
            const pcRetIdx = j;
            const pcRet = stack[j--];
            const pcRetStr = truncate(pcRet?.stringRep?.() ?? 'undefined', 57);

            // Previous SP (at sp - 5)
            const spPrevIdx = j;
            const spPrev = stack[j];

            // Sparse bit (at position before this frame's data started)
            const sparseIdx = j - 1;
            const sparseBit = sparseIdx >= 0 ? String(stack[sparseIdx]) : 'N/A';

            // Frame header
            if (frameNum > 0) {
                lines.push('─'.repeat(78));
            }
            lines.push(`Frame #${frameNum}`);

            // Frame fields - aligned with padding
            const field = (idx: number, name: string, value: string) => {
                const label = `  [${idx}] ${name}`.padEnd(20);
                return `${label}${value}`;
            };

            lines.push(field(branchBitIdx, 'Branch', branchStr));
            lines.push(field(mclearIdx, 'Mclear', mclearStr));
            lines.push(field(retIdx, 'Return', retString));
            lines.push(field(pcRetIdx, 'PC@ret', pcRetStr));
            lines.push(field(spPrevIdx, 'SP prev', String(spPrev)));
            lines.push(field(sparseIdx, 'Sparse', sparseBit));

            // Show locals if requested
            if (showLocals) {
                const localsStart = spPrev + CALLSIZE;
                const localsEnd = prevFrameSp - CALLSIZE;
                if (localsEnd > localsStart) {
                    for (let k = localsStart; k < localsEnd; k++) {
                        const localVal = stack[k];
                        let repr: string;
                        if (localVal?.stringRep) {
                            repr = localVal.stringRep();
                        } else if (localVal?.toString) {
                            repr = localVal.toString();
                        } else {
                            repr = String(localVal);
                        }
                        lines.push(field(k, 'Local', truncate(repr, 57)));
                    }
                }
            }

            prevFrameSp = spPrev;
            j = spPrev - 1;
            frameNum++;
        }

        if (frameNum >= maxDepth && j > 0) {
            lines.push('─'.repeat(78));
            lines.push(`... (${totalFrames - maxDepth} more frames not shown)`);
        }

        lines.push('─'.repeat(78));
        lines.push('');

        const output = lines.join('\n');
        console.log(output);
        return output;
    }

    


    addMonitor (pid, r) {
        this.monitors[r.val] = {pid: pid, uuid: r}
    }

    pcAtCreation(): Level {
        // The initial frame is at the bottom of the call stack
        // with structure: [sp_prev=0, pc, ret, mclear, branch_flag]
        // The PC is at index 1 (CALLSIZE - PCOFFSET = 5 - 4 = 1)
        return this.callStack[CALLSIZE - PCOFFSET];
    }

    private _validateDowngradeOrThrow(
        params: ValidateDowngradeParams
    ): void {
        const downgradeCheckResult: DowngradeResult =
            levels.okToDowngrade(params.downgradeKind, params.downgradeDimension)
                 (params.levFrom, params.levTo, params.authorityLevel, params.blockLevel as Level, this.isNmifcMode, this.pc);

        if (downgradeCheckResult.kind === "FAILURE") {
            try {
                const errorMessage = getDowngradeErrorMessage(params, downgradeCheckResult.reason);
                this.threadError(errorMessage, false, null, ErrorKind.IFCCheck);
            } catch (e) {
                if (e instanceof ImplementationError) {
                    this.threadError(e.message, true);
                } else {
                    throw e;
                }
            }
        }
    }

    tailCall (f, x) {
        this.setR0ToLValue (x);
        return f;
    }

    getSparseBit() {
        return this.callStack[this.sparseSlot]
    }

    invalidateSparseBit() {
        this.callStack[this.sparseSlot] = false;
    }

    private setSparseBit(b: boolean) {
        this.callStack[this.sparseSlot] = b;
    }

    /**
     * Check whether the label of R0 (argument), the data level of R0 and the given label are bound by PC.
     */
    updateSparseBitOnEntry(x: Level) {
        const _pc = this.pc 
        this.setSparseBit(
             flowsTo(this.r0_lev, _pc) 
             && flowsTo(x, _pc)
             // Only non-basic types (_troupeType is defined) have a data-level
             && (this.r0_val._troupeType == undefined || flowsTo (this.r0_val.dataLevel, _pc))
        )
    }

    /**
     * If the sparse bit is set, check whether it is still valid for the returned value:
     * Check whether the label of R0 (return value) and the data level of R0 are bound by PC.
     */
    updateSparseBitOnReturn() {
        const _pc = this.pc 
        if(this.getSparseBit()) { // only invalidating sparse bit
            this.setSparseBit( 
                flowsTo(this.r0_lev, _pc) 
             // Only non-basic types (_troupeType is defined) have a data-level
                    && (this.r0_val._troupeType == undefined || flowsTo (this.r0_val.dataLevel, _pc))
            )
        }
    }


    runNext (theFun, args, nm)  {
        this.next = () => {
            theFun.apply (nm, args);
        }
    }

    
   

    block(cb) {
        this.next = cb //  () => { return cb(); }
    }

    get arg_as_lval () {
        return new LVal (this.r0_val, this.r0_lev, this.r0_tlev);
    }

    pushFrame (cb, framesize=0) {
        let _prev_sp = this._sp
        this._sp = this._sp + framesize + CALLSIZE
        this.callStack[this._sp - SPOFFSET] = _prev_sp
        this.callStack[this._sp - PCOFFSET] = this.pc 
        this.callStack[this._sp - RETOFFSET] = cb 
        this.callStack[this._sp - MCLEAROFFSET] = this.mailbox.mclear 
        this.callStack[this._sp - BRANCHFLAGOFFSET] = BRANCH_FLAG_OFF

//        this.callStack.push (this.pc)
//        this.callStack.push ( cb ) 
//        this.callStack.push ( this.mailbox.mclear )
//        this.callStack.push (BRANCH_FLAG_OFF) 
//        this._sp += FRAMESIZE;

    }

    setBranchFlag () {
        this.callStack[this._sp - BRANCHFLAGOFFSET] = BRANCH_FLAG_ON
    }

    /**
     * Branch-balance discipline for the call-label channel: a call whose
     * function-value label does not flow to the current (pre-raise) PC is a
     * control transfer selected by data above the context — a branch for the
     * clearance discipline — so it sets the current frame's branch flag, and
     * the mailbox-clearance balance check at `returnImmediate` covers the
     * call. Called from generated code after the frame push and before the
     * call's PC raise; label-silent calls (in particular every public call)
     * do not flag.
     */
    setBranchFlagOnCallRaise (fnlev) {
        if (!flowsTo(fnlev, this.pc)) {
            this.setBranchFlag()
        }
    }
    
    returnSuspended (arg) {
        let rv = new LValCopyAt (arg, this.pc);
        this.next = () => {            
            return this.returnImmediateLValue (rv);
        }
    }

    throwInSuspended (arg) {
        this.next = () => {
            this.threadError (arg);
        }
    }

    setR0ToLValue (arg:LVal) {
        this.r0_val = arg.val
        this.r0_lev = arg.lev
        this.r0_tlev = arg.tlev
    }

    returnImmediateLValue (arg:LVal) {
        this.r0_val = arg.val
        this.r0_lev = arg.lev
        this.r0_tlev = arg.tlev
        return this.returnImmediate()
    }

    returnImmediate () {            
        let _STACK = this.callStack  
        let _SP = this._sp 

        let branchFlag = _STACK[_SP  - BRANCHFLAGOFFSET]
        let lclear = _STACK[_SP - MCLEAROFFSET]
        let ret = _STACK[_SP - RETOFFSET]

        // 2021-03-23; AA -- hack: in PINI mode the blocking label is restored back to the pc upon returns
        if (isPiniMode) {
            this.bl = this.pc
        }

        if (branchFlag) {
            if (lclear != this.mailbox.mclear) {
                // this.showStackV2 ()
                this.threadError (`Mailbox clearance label is not restorted after being raised in a branch; stack depth = ${this._sp}`, false, null, ErrorKind.IFCCheck)
            }
        }
        this.pc  = _STACK [_SP - PCOFFSET]; 
        this._sp = _STACK [_SP - SPOFFSET]; 
        this.next = ret        
        return ret;
    }

    
    mkUuidVal () {
        let pid = uuidv4();
        let uuidval = this.mkVal ( pid );
        return uuidval;  
    }  

    // TODO: deprecate(!) 2025-12-29; see comment below; AA

    pcpinipush ( auth: any, purpose: PCDowngradePurpose | string, bl = this.bl )  {
        let uid = uuidv4()
        let cap = this.mkVal (new Capability(uid,
                    { bl
                    , pc: this.pc
                    , auth : auth                    
                    , purpose: purpose
                    },
                    this.pini_uuid,
                    this.pc)); // 2021-05-12; AA; TODO: revisit this; alternative might be te use auth level? 
                               // also, why not block though that will require invalidating the
                               // sparse bit
                
        this.pini_uuid = uid;
        return this.returnImmediateLValue(cap)
    }

    // 2025-12-29: AA: this method 
    // is problemamtic in the context of the 
    // Stack representation that stores earlier 
    // PC values in "regular" raw escaping variables
    // TODO: deprecate (!)
    pcpop (cap_lval) {
        if (this.pini_uuid == null) {
            this.threadError ("unmatched pcpop", false, null, ErrorKind.IFCCheck);
        }

        let cap: Capability<any> = cap_lval.val;
        let {bl, pc, auth, purpose} = cap.data;

        // check the capability
        if (this.pini_uuid != cap.uid || purpose != PCDowngradePurpose.Full) {
            this.threadError ("Ill-scoped pinipush/pinipop", false, null, ErrorKind.IFCCheck);
            return null; // does not schedule anything in this thread
                         // effectively terminating/blocking the thread
        }

        
        // We declassify the current blocking level to the old blocking level. 
        // and also the current pc to the old pc. 
        // We check that there is sufficient authority to declassify from 
        // the current blocking level all the way down to the target pc 

        let levFrom = this.bl;
        let levTo = pc


        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // check that the provided authority is sufficient for the declassification
        this._validateDowngradeOrThrow({
            levFrom,
            levTo,
            authorityLevel: auth.val.authorityLevel,
            downgradeKind: DowngradeKind.BLOCKING,
            downgradeDimension: DowngradeDimension.BOTH,
            blockLevel: this.bl,
            operationDescription: "pc downgrade",
            pcLevel: this.pc
        });
        
        this.pc = pc;           
        this.bl = bl;
        let loop_sp = this._sp 
        let j = loop_sp - PCOFFSET; 
        while (j >= 0 && !levels.flowsTo (this.callStack[j], pc)) {   
            this.callStack[j] = pc;
            loop_sp = this.callStack[loop_sp - SPOFFSET]
            j = loop_sp - PCOFFSET 
        }            
        this.pini_uuid = cap.prev;
        
        this.invalidateSparseBit ()
         // 2025-12-29; 
         // thet above is poor man's attempt 
         // to mitigate for the havoc thath the 
         // stack traversal causes
         // but ultimately a failure. 
         // We should either have a very 
         // complicated "pc map" for cross-call escaping 
         // raw values that would need to be restored 
         // or just not do this
         // 
         // This compounds to the problem of 
         // PC pop + capabilities being a very 
         // adhoc mechanism in the first place
         // 
         // Let's try to write all the interesting 
         // programs we want to write without trying to
         // fix this and eventually deprecate this 
         // concept.

        return this.returnImmediateLValue (__unit); 
    }
    


    pinipop (cap_lval) {
        if (this.pini_uuid == null) {
            this.threadError ("unmatched pinipop", false, null, ErrorKind.IFCCheck);
        }

        debug (`Current pc level is ${this.pc.stringRep()}`)

        this.raiseBlockingThreadLev(this.pc); // maintaining the invariant that the blocking level is as high as the pc level

        let cap: Capability<any> = cap_lval.val;
        let {bl, pc, auth, purpose} = cap.data;


        if (this.pini_uuid != cap.uid || purpose != PCDowngradePurpose.Pini) {
            this.threadError ("Ill-scoped pinipush/pinipop", false, null, ErrorKind.IFCCheck);
            return; // does not schedule anything in this thread
                    // effectively terminating the thread
        }

        // If we are here then the pinipop is well-scoped
        // so we check the declassifications now

        let levFrom = this.bl;
        let levTo = bl;

        debug (`Level to declassify to at pinipop ${levTo.stringRep()}`)
        // this.showStack()
        // check that the provided authority is sufficient to perform declassification to the next level
        this._validateDowngradeOrThrow({
            levFrom,
            levTo,
            authorityLevel: auth.val.authorityLevel,
            downgradeKind: DowngradeKind.BLOCKING,
            downgradeDimension: DowngradeDimension.BOTH,
            operationDescription: "pini downgrading",
            pcLevel: this.pc
        });
        
        // Logic from former onSuccess callback
        this.bl = levTo ;
        this.pini_uuid = cap.prev;

        return this.returnImmediateLValue (__unit); 
    }

    blockEndorseTo (auth, bl_to = this.pc) {
        // 2025-05-30; AA
        // These are copy paste from declassify
        // we should recheck
        if (! flowsTo (this.pc, bl_to)) {
            this.threadError ("The provided target blocking level is lower than the current pc\n" +
                              ` | the current pc: ${this.pc.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }


        let ok_to_use = levels.flowsTo (auth.lev, bl_to);
        if (!ok_to_use) {
            this.threadError ("The provided authority value is tainted\n" +
                              ` | the level of the authority value: ${auth.lev.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }

        const current_bl = this.bl; // Capture this.bl as it's effectively levFrom

        this._validateDowngradeOrThrow({
            levFrom: current_bl,
            levTo: bl_to,
            authorityLevel: auth.val.authorityLevel,
            downgradeKind: DowngradeKind.BLOCKING,
            downgradeDimension: DowngradeDimension.INTEGRITY,
            blockLevel: current_bl,
            operationDescription: "blocking level integrity",
            pcLevel: this.pc
        });

        this.bl = bl_to; // the actual downgrade
        return this.returnImmediateLValue (__unit);

    }


    blockDeclassifyTo (auth, bl_to = this.pc, levOperandLabel = null) {
        if (! flowsTo (this.pc, bl_to)) {
            this.threadError ("The provided target blocking level is lower than the current pc\n" +
                              ` | the current pc: ${this.pc.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }


        let ok_to_use = levels.flowsTo (auth.lev, bl_to);
        if (!ok_to_use) {
            this.threadError ("The provided authority value is tainted\n" +
                              ` | the level of the authority value: ${auth.lev.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }

        // The choice of declassification target must not itself be secret with
        // respect to that target: the pc-declassification event is emitted at the
        // bare target level, so a high-labeled level operand would leak the choice
        // of target. Mirror of the authority-label check above, on the level
        // operand instead of the authority value.
        if (levOperandLabel !== null && ! flowsTo (levOperandLabel, bl_to)) {
            this.threadError ("The provided target level operand is tainted\n" +
                              ` | the level of the target level operand: ${levOperandLabel.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }

        const current_bl = this.bl; // Capture this.bl as it's effectively levFrom

        this._validateDowngradeOrThrow({
            levFrom: current_bl,
            levTo: bl_to,
            authorityLevel: auth.val.authorityLevel,
            downgradeKind: DowngradeKind.BLOCKING,
            downgradeDimension: DowngradeDimension.CONFIDENTIALITY,
            blockLevel: current_bl,
            operationDescription: "blocking level declassification",
            pcLevel: this.pc
        });

        this.bl = bl_to; // the actual downgrade
        return this.returnImmediateLValue (__unit);
    }

    // Cross-dimensional blocking level downgrade: changes both confidentiality and integrity
    blockDowngradeTo (auth, bl_to = this.pc) {
        if (! flowsTo (this.pc, bl_to)) {
            // TODO: 2026-03-10; AA -- change phrasing to use
            //   'does not flow to' instead of 'lower'
            this.threadError ("The provided target blocking level is lower than the current pc\n" +
                              ` | the current pc: ${this.pc.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }


        let ok_to_use = levels.flowsTo (auth.lev, bl_to);
        if (!ok_to_use) {
            this.threadError ("The provided authority value is tainted\n" +
                              ` | the level of the authority value: ${auth.lev.stringRep()}\n` +
                              ` | target blocking level: ${bl_to.stringRep()}`, false, null, ErrorKind.IFCCheck)
        }

        const current_bl = this.bl; // Capture this.bl as it's effectively levFrom

        this._validateDowngradeOrThrow({
            levFrom: current_bl,
            levTo: bl_to,
            authorityLevel: auth.val.authorityLevel,
            downgradeKind: DowngradeKind.BLOCKING,
            downgradeDimension: DowngradeDimension.BOTH,
            blockLevel: current_bl,
            operationDescription: "blocking level downgrade",
            pcLevel: this.pc
        });

        this.bl = bl_to; // the actual downgrade
        this.invalidateSparseBit ()
        return this.returnImmediateLValue (__unit);
    }

    raiseBlockingThreadLev (l) {                
        this.bl = lub (this.bl, l)        
    }

    raiseCurrentThreadPCToBlockingLev () {        
        this.pc = lub(this.pc, this.bl ) ;
    }

    raiseCurrentThreadPC (l)  {        
        this.pc = lub( this.pc, l )        
        this.raiseBlockingThreadLev(this.pc); 
            // 2018-11-29: AA; observe that we are raise the blocking level
            // automaticaly every time we raise the PC level.
    }



    mkVal(x) {
        return new LVal(x, this.pc, this.pc );
    }

    mkValPos(x: any, pos: string) {
        return new LVal (x, this.pc, this.pc, pos);
    }

    /*
    mkValWithTroupeType(x, t) {
        return new LVal (x, this.pc, this.pc, null, t)
    }
    */

    mkValWithLev(x:any, l:any) {                    
        return new LVal ( x
                        , lub(this.pc, l)
                        , this.pc )              
    }

    mkCopy (x) {
        return new LValCopyAt (x, this.pc);
        // return new LVal(x.val, lub(x.lev, this.pc), lub (x.tlev, this.pc) )
    }

    
    printPc () {
        console.log ("PC:", this.pc.stringRep());
        console.log ("BL:", this.bl.stringRep());
    }


    tidErrorStringRep() {
        if (this.processDebuggingName) {
            return ("[" + this.processDebuggingName + "]" + this.tid.stringRep() )
        }   
        else {
            return this.tid.stringRep()
        }
    }


    threadError (s:string, internal = false, explainer = null, errorKind: ErrorKind = ErrorKind.DynTypeError) {
        if ( this.handlerState.isNormal()) {
          if (internal)  {
            throw new ImplementationError(s)
          }
          else {
            throw new StrThreadError(this, s, explainer, errorKind);
          }
        } else {
          this.raiseCurrentThreadPC(this.handlerState.lev);
          throw new HandlerError (this, s, errorKind)
        }
    }
   
    threadErrorWithExplainer (s : string, explainer: () => string) {
        if (getCliArgs()[TroupeCliArg.Explain] && explainer) {
            this.threadError (s, false, explainer ());         
        } else {
            this.threadError (s);
        }
    }
    
    addMessage (message) {
        this.mailbox.newMessage (message);    
    }

    // Ranged-receive: open a clearance region ⟨lo, hi⟩ whose close is certified up front
    // by the shown authority. Never refuses (beyond the builtin's type checks). Returns
    // the pair (ok_to_dg, cap); when the shown authority does not cover restoring the
    // mailbox view from hi down to lo, ok_to_dg is false and the capability is invalid —
    // the region is pushed all the same, as an ordinary region that never closes (its
    // disable fails on validity, and the LIFO chain-head check blocks every enclosing
    // close while it is open — exactly like a legacy raise no authority can lower).
    enableRangedReceive (lo:any, hi:any, auth:any) {
        const mc = this.mailbox.mclear;
        const Delta = mc.delta;                     // the ambient active ceiling BEFORE this enable
        const authLevel = auth.val.authorityLevel;

        // Certification, evaluated at the open: may (hi ⊔ Δ) flow to (lo ⊔ Δ) under the
        // shown authority? Sound to decide here because the LIFO discipline makes the
        // ambient active ceiling at the matching disable exactly this Δ, so the check decided now
        // is the check that would be decided then. The decision goes through the SAME
        // pure downgrade-decision function every other downgrade runs (okToDowngrade with
        // the mailbox kind and the cross-dimensional target, exactly as the legacy
        // every other mailbox downgrade ran) — so with NMIFC on the decision
        // inherits the robust-declassification / transparent-endorsement discipline by
        // construction; with NMIFC off it is the plain privilege relation (privFlowsTo).
        // Unlike the legacy path the enable never throws: an unfavourable decision
        // degrades to ok_to_dg = false (an ordinary region that never closes).
        const levFrom = lub (hi.val, Delta);
        const levTo   = lub (lo.val, Delta);
        const dgDecision: DowngradeResult =
            levels.okToDowngrade (DowngradeKind.MAILBOX, DowngradeDimension.BOTH)
                  (levFrom, levTo, authLevel, this.bl, this.isNmifcMode, this.pc);
        const okToDg = dgDecision.kind === "SUCCESS";
        const dgReason = dgDecision.kind === "FAILURE" ? dgDecision.reason : null;

        // Blocking-label quarantine: the enable's operand match is a blocking
        // decision (a secret-labelled operand whose constructor diverges across runs
        // opens the region in one run and sticks in the other), so the operand data
        // labels quarantine the thread's blocking label:
        // bl ⊔= ld(lo) ⊔ ld(hi) ⊔ ld(auth).
        this.raiseBlockingThreadLev (lub (lo.lev, hi.lev, auth.lev));

        // Both returned components are labelled pc ⊔ ld(lo) ⊔ ld(hi) ⊔ ld(auth) ⊔ Δlab ⊔ Φlab.
        // The range-label terms are necessary: the certification bit consults Δ, and the
        // region push joins the active range, whose bounds come from the enclosing
        // enables' operands — so the result must carry their labels.
        const capLabel = lub (this.pc, lo.lev, hi.lev, auth.lev, mc.deltaLab, mc.phiLab);

        // One uniform push path, certified or not: chain the capability, join the active range;
        // the capability snapshots the pre-enable record so a (valid) disable restores it
        // by identity. The only difference for an uncertified region is valid = false.
        const uid = uuidv4();
        const capObj = new Capability (uid,
            new RangedReceiveCap (mc, this.pc, lo.val, okToDg, levFrom, levTo, authLevel, dgReason),
            this.mailbox.caps, capLabel);
        this.mailbox.caps = uid;
        this.mailbox.mclear = mc.copyWith ({ folds: {
            delta:    lub (mc.delta,    hi.val),
            phi:      lub (mc.phi,      lo.val),
            deltaLab: lub (mc.deltaLab, hi.lev),
            phiLab:   lub (mc.phiLab,   lo.lev),
        }});

        const okLval  = new LVal (okToDg, capLabel, capLabel);
        const capLval = new LVal (capObj, capLabel, capLabel);
        const tuple   = mkTuple ([okLval, capLval]);
        return this.returnImmediateLValue (new LVal (tuple, capLabel, capLabel));
    }

    // Ranged-receive: close a region opened by enableRangedReceive. Authority-free — the
    // capability is the certificate. Order of attribution: occurrence, validity, LIFO.
    disableRangedReceive (cap_lval:any) {
        const cap: Capability<RangedReceiveCap> = cap_lval.val;
        const data = cap.data;

        // (a) Occurrence — hard: pc ⊔ ld(cap) ⊑ pc_enable AND pc ⊔ ld(cap) ⊑ lo; bl absorbs
        // ld(cap) BEFORE any branching on the capability, so a secret-selected capability
        // cannot make the disable's outcome observable below the secret.
        this.raiseBlockingThreadLev (cap_lval.lev);
        if (!levels.flowsTo (lub (this.pc, cap_lval.lev), data.pc_enable)) {
            this.threadError ("The pc at the disableRangedReceive must flow to the pc at the enableRangedReceive.\n" +
                              ` | pc level at the disableRangedReceive : ${this.pc.stringRep()}\n` +
                              ` | pc level at the enableRangedReceive  : ${data.pc_enable.stringRep()}\n` +
                              ` | level of the capability              : ${cap_lval.lev.stringRep()}`, false, null, ErrorKind.IFCCheck);
        }
        // The release level: the close's observable restoration lands at the region's
        // floor lo, so its occurrence must not depend on anything above lo — a region
        // enabled at a high pc with a low floor must not close inside the high context
        // (every emitting step must have pc below the event level).
        if (!levels.flowsTo (lub (this.pc, cap_lval.lev), data.lo)) {
            this.threadError ("The pc at the disableRangedReceive must flow to the lower bound of the enableRangedReceive.\n" +
                              ` | pc level at the disableRangedReceive   : ${this.pc.stringRep()}\n` +
                              ` | lower bound of the enableRangedReceive : ${data.lo.stringRep()}\n` +
                              ` | level of the capability                : ${cap_lval.lev.stringRep()}`, false, null, ErrorKind.IFCCheck);
        }

        // (b) Validity — the enable's certification refused, so this capability has no
        // close. The enable did not report it then (it returns the decision instead of
        // throwing), so the refusal is explained here, from the operands the enable
        // weighed, through the same formatter every other downgrade uses.
        if (!data.valid) {
            const params: ValidateDowngradeParams = {
                downgradeKind: DowngradeKind.MAILBOX,
                downgradeDimension: DowngradeDimension.BOTH,
                levFrom: data.levFrom,
                levTo: data.levTo,
                authorityLevel: data.authLevel,
                blockLevel: this.bl,
                pcLevel: this.pc,
                operationDescription: "disableRangedReceive"
            };
            try {
                this.threadError (getDowngradeErrorMessage (params, data.dgReason), false, null, ErrorKind.IFCCheck);
            } catch (e) {
                if (e instanceof ImplementationError) { this.threadError (e.message, true); }
                else { throw e; }
            }
        }

        // (c) LIFO scoping — the capability must be the head of the chain.
        if (this.mailbox.caps == null) {
            this.threadError ("disableRangedReceive has no open enableRangedReceive to close.", false, null, ErrorKind.IFCCheck);
        }
        if (this.mailbox.caps != cap.uid) {
            this.threadError ("disableRangedReceive must close the last enableRangedReceive first.\n" +
                              ` | capability of the last enableRangedReceive : ${this.mailbox.caps}\n` +
                              ` | capability provided                        : ${cap.uid}`, false, null, ErrorKind.IFCCheck);
        }

        // (d) No authority check — the downgrade was certified at the enable. Pop: restore
        // the enable-time record by identity and the previous capability-chain head. The
        // verbatim restore is right under the uniform LIFO discipline: every region,
        // certified or not, sits on the chain, so a pop only happens with everything
        // above it closed — an uncertified region above would have failed this disable
        // at the chain-head check.
        this.mailbox.mclear = data.mclearSnapshot;
        this.mailbox.caps = cap.prev;
        return this.returnImmediateLValue (__unit);
    }
}

