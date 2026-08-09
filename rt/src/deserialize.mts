"use strict";
import { strict as assert } from 'node:assert'
import {spawn} from 'child_process'
import { v4 as uuidv4 } from 'uuid';
import * as Ty from './TroupeTypes.mjs'
import { __exitInitiated } from './runtimeMonitored.mjs';
import { LVal } from './Lval.mjs';
import { mkTuple, mkList } from './ValuesUtil.mjs';
import { isWellFormedSynVariant } from './RawTuple.mjs';
import { ProcessID } from './process.mjs';
import { Authority } from './Authority.mjs';
import { TroupeBigInt } from './TroupeBigInt.mjs';
import { __unitbase }from './UnitBase.mjs'
import { mkLevel } from './Level.mjs';
import { RuntimeInterface } from './RuntimeInterface.mjs';
import { Level } from './Level.mjs';
import { Record } from './Record.mjs';
import { RawClosure } from './RawClosure.mjs';
import * as levels from './Level.mjs';
import { getTroupeRoot } from './troupeRoot.mjs';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { mkLogger } from './logger.mjs';
import { DCLabel, createQuarantineAuthority, QuarantineTag } from './levels/DCLabels/dclabel.mjs';
import { __nodeManager } from './NodeManager.mjs';
import {
    isRegularTrust,
    classifyForIngress,
    IngressClassification
} from './Ingress.mjs';
import { TroupeError } from './TroupeError.mjs';
import { NativeUnavailableError } from './ffi/registry.mjs';
import { SchedulerInterface } from './SchedulerInterface.mjs';

const argv = getCliArgs();
const logLevel = argv[TroupeCliArg.DebugQuarantine] ? 'debug' : 'info';
const logger = mkLogger('QRN', logLevel);
const qdebug = (x: string) => logger.debug(x);
const dlogger = mkLogger('deserialize');

// Ingress check result types for quarantine protocol
export enum IngressResult {
    TRUSTED,      // All labels trusted - use original value
    QUARANTINE,   // Some labels untrusted - apply quarantine label
    DROP          // Corrupt label found - drop message
}

export type DeserializeResult = {
    result: IngressResult;
    value?: LVal;                   // Present if TRUSTED or QUARANTINE
    quarantineAuth?: Level;         // Present if QUARANTINE
}

// Exception thrown when corrupt data is encountered during deserialization
class CorruptDataException extends Error {
    constructor() {
        super("Corrupt data encountered during deserialization");
    }
}

let __compilerOsProcess = null;

let __rtObj = null;

// obs: these are global...
let __isCurrentlyUsingCompiler = false; // simple flag to make sure we handle one deserialization at a time
let __currentCallback = null;           // a callback for synchronizing with the caller
let __currentErrback = null;            // rejects the pending deserialization on failure
let __currentDeserializedJson = null;
let __trustLevel = null;


export function setRuntimeObj(rt: RuntimeInterface) {
    __rtObj = rt;
}

const HEADER:string =
        "this.libSet = new Set () \n\
         this.libs = [] \n\
         this.addLib = function (lib, decl)\
             { if (!this.libSet.has (lib +'.'+decl)) {  \
             this.libSet.add (lib +'.'+decl);\
             this.libs.push ({lib:lib, decl:decl})} }\n"

// Merge multiple source maps into a single source map object
// Each source map has: { file, sources, sourcesContent, names, mappings, version }
function mergeSourceMaps(sourceMaps: any[]): any {
    if (sourceMaps.length === 0) return null
    if (sourceMaps.length === 1) return sourceMaps[0]

    // For dynamically loaded code, we primarily care about the mappings
    // Combine all sources and mappings from each source map
    const sources: string[] = []
    const mappings: string[] = []

    for (const sm of sourceMaps) {
        if (sm.sources) {
            for (const src of sm.sources) {
                if (!sources.includes(src)) {
                    sources.push(src)
                }
            }
        }
        if (sm.mappings) {
            mappings.push(sm.mappings)
        }
    }

    return {
        version: 3,
        file: "",
        sources: sources,
        mappings: mappings.join(";")
    }
}

function startCompiler() {
    __compilerOsProcess = spawn(getTroupeRoot() + '/bin/troupec', ['--json-ir']);
    __compilerOsProcess.on('exit', (code: number) => {
        // Don't exit if runtime's exit() was already called - it will handle the exit code
        if (!__exitInitiated) {
            process.exit(code);
        }
    });

    let marker = "/*-----*/\n\n"

    // accumulator of communication with the compiler; reset after
    // each deserialization; needed because we have no guarantees about
    // how the data coming back from the compiler is chunked

    let accum = "";

    __compilerOsProcess.stdout.on('data', (data: string) => {
        accum += data;
        let j = accum.indexOf(marker);
        if (j >= 0) {
            constructCurrent(accum.slice(0, j));
            accum = accum.slice(j + marker.length);
        }
    });
}

startCompiler();

export function stopCompiler() {
    __compilerOsProcess.stdin.end();
}


// --------------------------------------------------

// some rudimentary debugging mechanisms; probably should be rewritten
function debuglog(...s) {
    let spaces = "";
    for (let j = 0; j < indentcounter; j++) {
        spaces = "  " + spaces;
    }

    s.unshift("DEBUG:" + spaces)
    console.log.apply(null, s)
}

var indentcounter = 0;

function indent() {
    indentcounter++;
}

function unindent() {
    indentcounter--;
}



// Error raised when an inbound serialized value cannot be reconstructed.
// `deserialize` rejects its promise with this error; callers surface it as a
// thread-level error, which the scheduler's TroupeError path handles. If it
// ever propagates into the scheduler directly, it is logged there as well.
export class DeserializationError extends TroupeError {
    handleError(sched: SchedulerInterface): void {
        dlogger.error(this.message);
    }
}

function asDeserializationError(cause: unknown): DeserializationError {
    if (cause instanceof DeserializationError) {
        return cause;
    }
    const details = cause instanceof Error ? cause.message : String(cause);
    return new DeserializationError(`cannot deserialize inbound value: ${details}`);
}

// The list of exceptions expected when processing untrusted inbound data: a
// value we cannot reconstruct or link (e.g. a closure naming a module,
// library, or native module this node does not have). A receiving boundary
// drops these and keeps serving. This is deliberately narrow by type but wide
// in intent: adversarial inputs are expected here, so they must be handled
// rather than crash the node. Anything not on this list is unexpected and must
// surface (see the node's fail-fast policy), so a genuine bug is exposed
// rather than silently swallowed. Extend this predicate as further expected
// inbound-error classes are added.
export function isExpectedInboundError(e: unknown): boolean {
    return e instanceof DeserializationError
        || e instanceof NativeUnavailableError;
}

// Wrapper around the reconstruction logic: any failure to reconstruct an
// inbound value is reported through the current errback (rejecting the
// promise returned by `deserialize`) instead of crashing the node.
function constructCurrent(compilerOutput: string) {
    try {
        constructCurrentUnchecked(compilerOutput);
    } catch (e) {
        __currentErrback(asDeserializationError(e));
    }
}

function constructCurrentUnchecked(compilerOutput: string) {
    __isCurrentlyUsingCompiler = false;
    let serobj = __currentDeserializedJson;
    let desercb = __currentCallback;
    let deserErrback = __currentErrback;

    // 1. reconstruct the namespaces
    let snippets = compilerOutput.split("\n\n");
    let k = 0;


    let ctxt = { // deserialization context 
        namespaces : new Array (serobj.namespaces.length),
        closures   : new Array (serobj.closures.length),
        envs       : new Array (serobj.envs.length)
    }

    for (let i = 0; i < serobj.namespaces.length; i++) {
        let ns = serobj.namespaces[i]
        let nsFun = HEADER

        // Collect source maps from all snippets in this namespace
        let namespaceMappings: any[] = []

        for (let j = 0; j < ns.length; j++) {
            if (j > 0) {
                nsFun += "\n\n" // looks neater this way
            }
            let snippetJson = JSON.parse(snippets[k++]);
            nsFun += snippetJson.code;

            // Collect source map from snippet if available
            if (snippetJson.sourceMap) {
                namespaceMappings.push(snippetJson.sourceMap)
            }
        }
        let argNames: string[] = ['rt', nsFun]
        // The namespace function takes only the runtime object as its argument.
        let NS: any = Reflect.construct (Function, argNames)

        // We now construct an instance of the newly constructed object
        // that takes the runtime object as its argument
        let argValues: any[] = [__rtObj]
        ctxt.namespaces[i] = Reflect.construct (NS, argValues)
        // Mark namespace as restored code for error reporting
        Object.defineProperty(ctxt.namespaces[i], '__isDynamic', {
            value: true,
            enumerable: false
        })
        // Attach merged source map to namespace for error position translation
        if (namespaceMappings.length > 0) {
            // Merge source maps by combining their mappings field
            const mergedSourceMap = mergeSourceMaps(namespaceMappings)
            Object.defineProperty(ctxt.namespaces[i], '__sourceMap', {
                value: mergedSourceMap,
                enumerable: false
            })
        } 
        
    }

    /*
                   #     #
     #    # #    # #     #   ##   #      #    # ######
     ##  ## #   #  #     #  #  #  #      #    # #
     # ## # ####   #     # #    # #      #    # #####
     #    # #  #    #   #  ###### #      #    # #
     #    # #   #    # #   #    # #      #    # #
     #    # #    #    #    #    # ######  ####  ######

    */

    // IngressDeserializer performs the ingress check during deserialization.
    // Each instance tracks whether any label was quarantined.
    class IngressDeserializer {
        private _quarantineTag: QuarantineTag | null = null;
        private _quarantineAuth: Level | null = null;

        /** Lazy getter - creates quarantine tag on first access */
        get quarantineTag(): QuarantineTag {
            if (this._quarantineTag === null) {
                // Use sender's node ID if available, otherwise fall back to receiver's node ID.
                // Using sender's node ID allows quarantined data to be sent back to the sender.
                const nodeId = __senderNodeId ?? __nodeManager.getNodeId();
                this._quarantineTag = {
                    nodeId: nodeId,
                    quarantineId: uuidv4().toString()
                };
            }
            return this._quarantineTag;
        }

        /** Returns the quarantine authority (qfalse-based) if quarantine occurred */
        get quarantineAuthority(): Level | null {
            if (this._quarantineTag === null) return null;
            if (this._quarantineAuth === null) {
                this._quarantineAuth = createQuarantineAuthority(this._quarantineTag);
            }
            return this._quarantineAuth;
        }

        /** Returns true if any label was quarantined */
        get wasQuarantined(): boolean {
            return this._quarantineTag !== null;
        }

        /**
         * Check label and return effective label based on three-case ingress logic.
         *
         * Cases:
         * - TRUSTED: Both I and C within trust - use original label
         * - FULL_OVERCLAIM: Neither I nor C within trust - quarantine both
         * - INTEGRITY_OVERCLAIM: C within trust, I exceeds - consult CLI setting
         */
        private checkLabel(lev: Level): Level {
            const dcLevel = lev as DCLabel;
            const trustDC = __trustLevel as DCLabel;

            // First check corruption (applies to all cases)
            if (dcLevel.isCorrupt()) {
                qdebug(`DROP: corrupt label ${lev.stringRep()}`);
                throw new CorruptDataException();
            }

            // Check if trust level is regular (I_n <=> C_n)
            // If not regular, fall back to legacy behavior
            if (!isRegularTrust(trustDC)) {
                return this.checkLabelLegacy(lev);
            }

            // Classify the label against trust level
            const classification = classifyForIngress(dcLevel, trustDC);

            switch (classification) {
                case IngressClassification.TRUSTED:
                    // Both I and C within trust - use original
                    qdebug(`TRUSTED: label ${lev.stringRep()} within trust ${__trustLevel.stringRep()}`);
                    return lev;

                case IngressClassification.FULL_OVERCLAIM:
                    // Neither I nor C within trust - quarantine both
                    const quarantinedLabel = dcLevel.quarantine(this.quarantineTag);
                    qdebug(`QUARANTINE (full): label ${lev.stringRep()} -> ${quarantinedLabel.stringRep()}`);
                    return quarantinedLabel;

                case IngressClassification.INTEGRITY_OVERCLAIM:
                    // C within trust, I exceeds - quarantine both (same as full overclaim)
                    const quarantinedIntegrity = dcLevel.quarantine(this.quarantineTag);
                    qdebug(`QUARANTINE (integrity): label ${lev.stringRep()} -> ${quarantinedIntegrity.stringRep()}`);
                    return quarantinedIntegrity;
            }
        }

        /**
         * Legacy behavior for non-regular trust levels.
         * Uses simple actsFor check.
         */
        private checkLabelLegacy(lev: Level): Level {
            if (levels.actsFor(__trustLevel, lev)) {
                qdebug(`TRUSTED (legacy): label ${lev.stringRep()}`);
                return lev;
            }
            const quarantinedLabel = (lev as DCLabel).quarantine(this.quarantineTag);
            qdebug(`QUARANTINE (legacy): label ${lev.stringRep()} -> ${quarantinedLabel.stringRep()}`);
            return quarantinedLabel;
        }

        private deserializeArray(x: any[]): LVal[] {
            let a: LVal[] = [];
            for (let i = 0; i < x.length; i++) {
                a.push(this.mkValue(x[i]));
            }
            return a;
        }

        /** Main deserialization method - adapted from existing mkValue */
        mkValue(arg: { val: any; lev: any; tlev: any; troupeType: Ty.TroupeType; }): LVal {
            // debuglog ("*** mkValue", arg);
            assert(Ty.isLVal(arg));
            let obj = arg.val;
            let lev = mkLevel(arg.lev);
            let tlev = mkLevel(arg.tlev);

            const effectiveLev = this.checkLabel(lev);
            const effectiveTlev = this.checkLabel(tlev);

            let _tt = arg.troupeType;

            const value = (): any => {
                switch (_tt) {
                    case Ty.TroupeType.RECORD:
                        // for records, the serialization format is  [[key, value_json], ...]
                        let a: [string, LVal][] = [];
                        for (let i = 0; i < obj.length; i++) {
                            a.push([obj[i][0], this.mkValue(obj[i][1])]);
                        }
                        return Record.mkRecord(a);
                    case Ty.TroupeType.LIST:
                        return mkList(this.deserializeArray(obj));
                    case Ty.TroupeType.TUPLE: {
                        const isSynVariant = obj.isSynVariant === true;
                        const vals = this.deserializeArray(obj.vals);
                        // A flagged tuple must carry the (tag) / (tag, payload)
                        // shape; a malformed one is corrupt inbound data and is
                        // dropped like any other, rather than crashing the
                        // printer later.
                        if (isSynVariant && !isWellFormedSynVariant(vals)) {
                            throw new CorruptDataException();
                        }
                        return mkTuple(vals, isSynVariant);
                    }
                    case Ty.TroupeType.CLOSURE:
                        return mkClosure(obj.ClosureID);
                    case Ty.TroupeType.NUMBER:
                    case Ty.TroupeType.BOOLEAN:
                    case Ty.TroupeType.STRING:
                        return obj;
                    case Ty.TroupeType.PROCESS_ID:
                        return new ProcessID(obj.uuid, obj.pid, obj.node);
                    case Ty.TroupeType.AUTHORITY:
                        // 2018-10-18: AA: authority attenuation based on the trust level of the sender
                        // Use checkLabel - if trusted, keep original authority; if not, quarantine it
                        return new Authority(this.checkLabel(mkLevel(obj.authorityLevel)));
                    case Ty.TroupeType.LEVEL:
                        return mkLevel(obj.lev);
                    case Ty.TroupeType.LVAL:
                        return this.mkValue(obj);
                    case Ty.TroupeType.BIGINT:
                        // wire form is the decimal string (see serialize.mts)
                        return new TroupeBigInt(BigInt(obj));
                    case Ty.TroupeType.UNIT:
                        return __unitbase;
                    default:
                        return obj;
                }
            };

            return new LVal(value(), effectiveLev, effectiveTlev);
        }
    }

    // Create ingress checker instance for this deserialization operation
    const ingress = new IngressDeserializer();

    // 2. reconstruct the closures and environments
    let sercloss = serobj.closures;

    let serenvs = serobj.envs;

    function mkClosure(i: number) {
        if (!ctxt.closures[i]) {
            let nm = ctxt.namespaces[sercloss[i].namespacePtr.NamespaceID]
            let fn = nm[sercloss[i].fun];
            let env = mkEnv(sercloss[i].envptr.EnvID, (env: any) => {
                ctxt.closures[i] = RawClosure(env, nm, fn);
            })
            ctxt.closures[i].__dataLevel = env.__dataLevel;
        }
        return ctxt.closures[i];
    }

    function mkEnv(i: number, post_init?: (arg0: any) => void) {
        if (!ctxt.envs[i]) {
            let env: any = { __dataLevel: levels.BOT };
            if (post_init) {
                post_init(env);
            }
            ctxt.envs[i] = env;
            for (var field in serenvs[i]) {
                let v = ingress.mkValue(serenvs[i][field]);
                env[field] = v;
                env.__dataLevel = levels.lub(env.__dataLevel, v.dataLevel);
            }
        } else {
            if (post_init) {
                post_init(ctxt.envs[i]);
            }
        }
        return ctxt.envs[i];
    }

    for (let i = 0; i < sercloss.length; i++) {
        mkClosure(i);
    }

    for (let i = 0; i < serenvs.length; i++) {
        mkEnv(i);
    }

    // Deserialize the main value with exception handling for corrupt data
    let result: DeserializeResult;
    try {
        let v = ingress.mkValue(serobj.value);
        if (ingress.wasQuarantined) {
            result = {
                result: IngressResult.QUARANTINE,
                value: v,
                quarantineAuth: ingress.quarantineAuthority
            };
        } else {
            result = { result: IngressResult.TRUSTED, value: v };
        }
    } catch (e) {
        if (e instanceof CorruptDataException) {
            result = { result: IngressResult.DROP };
        } else {
            throw e;  // Re-throw unexpected errors
        }
    }

    // go over the namespaces we have generated
    // and load all libraries before calling the last callback

    function loadLib(i: number, cb) {
        if (i < ctxt.namespaces.length) {
            // Relinking a received closure's libraries can fail cleanly — most
            // notably when it names a module the receiver does not have (not
            // locally discoverable). Route that to the deserialization errback
            // so it surfaces as a DeserializationError rather than an unhandled
            // rejection that crashes the node.
            __rtObj.linkLibs(ctxt.namespaces[i])
                   .then(() => loadLib(i + 1, cb))
                   .catch((e) => deserErrback(asDeserializationError(e)))
        } else {
            cb();
        }
    }

    loadLib(0, () => desercb(result));
}

// 2018-11-30: AA: TODO: implement a proper deserialization queue instead of
// the coarse-grained piggybacking on the event loop

let __senderNodeId: string | undefined = undefined;

function deserializeCb(lev: Level, jsonObj: any, senderNodeId: string | undefined,
                       cb: (result: DeserializeResult) => void,
                       errb: (err: DeserializationError) => void) {
    if (__isCurrentlyUsingCompiler) {
        setImmediate(deserializeCb, lev, jsonObj, senderNodeId, cb, errb) // postpone; 2018-03-04;aa
    } else {
        __senderNodeId = senderNodeId;
        __isCurrentlyUsingCompiler = true // prevent parallel deserialization attempts; important! -- leads to nasty
        // race conditions otherwise; 2018-11-30; AA
        __trustLevel = lev;
        __currentCallback = cb;      // obs: this is a global for this module;
        // the access to it should be carefully controlled
        __currentErrback = errb;

        // we need to share this object with the callbacks

        __currentDeserializedJson = jsonObj; // obs: another global that we must be careful with

        try {
            if (jsonObj.namespaces.length > 0) {
                for (let i = 0; i < jsonObj.namespaces.length; i++) {
                    let ns = jsonObj.namespaces[i];
                    for (let j = 0; j < ns.length; j++) {
                        // debuglog("*s deserialize", ns[j]);
                        __compilerOsProcess.stdin.write(ns[j][1]);
                        __compilerOsProcess.stdin.write("\n")
                        // debuglog ("data out")
                    }
                }
                __compilerOsProcess.stdin.write("!ECHO /*-----*/\n")
            } else {
                // shortcutting the unnecessary interaction with the compiler
                // 2018-09-20: AA
                constructCurrent("");
            }
        } catch (e) {
            __isCurrentlyUsingCompiler = false;
            errb(asDeserializationError(e));
        }
    }
}

/**
 * Deserialize a value from JSON with ingress checking.
 *
 * @param lev The trust level for the sender
 * @param jsonObj The serialized JSON object
 * @param senderNodeId Optional sender node ID. If provided and labels need quarantining,
 *                     the quarantine tag will use this ID (allowing the data to be sent
 *                     back to the sender). If not provided, uses the receiver's node ID.
 */
export function deserialize(lev: Level, jsonObj: any, senderNodeId?: string): Promise<DeserializeResult> {
    return new Promise((resolve, reject) => {
        deserializeCb(lev, jsonObj, senderNodeId, (result: DeserializeResult) => {
            resolve(result)
        }, reject)
    });
}
