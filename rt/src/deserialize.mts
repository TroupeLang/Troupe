"use strict";
import { strict as assert } from 'node:assert'
import {spawn} from 'child_process'
import * as Ty from './TroupeTypes.mjs'
import { LVal } from './Lval.mjs';
import { mkTuple, mkList } from './ValuesUtil.mjs';
import { ProcessID } from './process.mjs';
import { Authority } from './Authority.mjs';
import { Atom } from './Atom.mjs';
import { __unitbase }from './UnitBase.mjs'
import { glb, mkLevel } from './options.mjs';
import { RuntimeInterface } from './RuntimeInterface.mjs';
import { Level } from './Level.mjs';
import { Record } from './Record.mjs';
import { RawClosure } from './RawClosure.mjs';
import * as levels from './levels/tagsets.mjs';

let __compilerOsProcess = null;

let __rtObj = null;

// obs: these are global...
let __isCurrentlyUsingCompiler = false; // simple flag to make sure we handle one deserialization at a time
let __currentCallback = null;           // a callback for synchronizing with the caller
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

function startCompiler() {
    __compilerOsProcess = spawn(process.env.TROUPE + '/bin/troupec', ['--json']);
    __compilerOsProcess.on('exit', (code: number) => {
        process.exit(code);
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



function deserializationError() {
    console.log("DESERIALIZATION ERROR HANDLING IS NOT IMPLEMENTED")
    process.exit(1);
}

function constructCurrent(compilerOutput: string) {
    // debuglog (deserializationObject)

    __isCurrentlyUsingCompiler = false;
    let serobj = __currentDeserializedJson;
    let desercb = __currentCallback;

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

        let atomSet = new Set<string>()

        // nsFun += "this.libSet = new Set () \n"
        // nsFun += "this.libs = [] \n"
        // nsFun += "this.addLib = function (lib, decl) " +
        //     " { if (!this.libSet.has (lib +'.'+decl)) { " +
        //     " this.libSet.add (lib +'.'+decl); " +
        //     " this.libs.push ({lib:lib, decl:decl})} } \n"
        // nsFun += "this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) } \n"


        for (let j = 0; j < ns.length; j++) {
            if (j > 0) {
                nsFun += "\n\n" // looks neater this way
            }
            let snippetJson = JSON.parse(snippets[k++]);
            // console.log (snippetJson.libs);
            // console.log (snippetJson.fname);
            nsFun += snippetJson.code;

            for (let atom of snippetJson.atoms) {
                atomSet.add(atom)
            }
            // console.log (snippetJson.atoms)
        }
        let argNames = Array.from(atomSet);
        let argValues = argNames.map( argName => {return new Atom(argName)})
        argNames.unshift('rt')        
        argNames.push(nsFun)        
        // Observe that there is some serious level of 
        // reflection going on in here 
        //    Arguments to Function are 
        //             'rt', ATOM1, ..., ATOMk, nsFun 
        //    
        // 
        let NS: any = Reflect.construct (Function, argNames)

        // We now construct an instance of the newly constructed object
        // that takes the runtime object + atoms as its arguments

        // console.log (NS.toString()); // debugging
        argValues.unshift(__rtObj)
        ctxt.namespaces[i] = Reflect.construct (NS, argValues) 
        
    }

    // 2. reconstruct the closures and environments
    let sercloss = serobj.closures;

    let serenvs = serobj.envs;

    function mkClosure(i: number) {
        if (!ctxt.closures[i]) {            
            let nm = ctxt.namespaces[sercloss[i].namespacePtr.NamespaceID]
            let fn = nm[sercloss[i].fun];              
            let env = mkEnv(sercloss[i].envptr.EnvID, (env) => {                
                ctxt.closures[i] = RawClosure(env, nm, fn);
            })        
            ctxt.closures[i].__dataLevel = env.__dataLevel;     
        }
        return ctxt.closures[i];
    }

    function mkEnv(i: number, post_init?: (any)=>void ) {
        if (!ctxt.envs[i]) {        
            let env = {__dataLevel : levels.BOT};
            if (post_init) {
                post_init (env)
            }
            ctxt.envs[i] = env;
            for (var field in serenvs[i]) {
                let v = mkValue(serenvs[i][field]);
                env[field] = v 
                env.__dataLevel = levels.lub (env.__dataLevel, v.dataLevel)
            }            
        } else {
            if (post_init) {
                post_init (ctxt.envs[i]);
            }
        }
        return ctxt.envs[i]
    }


    function deserializeArray(x) {
        let a = [];
        for (let i = 0; i < x.length; i++) {
            a.push(mkValue(x[i]));
        }
        return a 
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

    function mkValue(arg: { val: any; lev: any; tlev: any; troupeType: Ty.TroupeType; }) {
        // debuglog ("*** mkValue", arg);
        assert(Ty.isLVal(arg));
        let obj = arg.val;
        let lev = mkLevel(arg.lev);
        let tlev = mkLevel(arg.tlev);

        function _trustGLB(x: Level) {
            return (glb(x, __trustLevel))
        }

        let _tt = arg.troupeType
    

        function value() {            
            switch (_tt) {                
                case Ty.TroupeType.RECORD:
                    // for reords, the serialization format is  [[key, value_json], ...]
                    let a = [];
                    for (let i = 0; i < obj.length; i++) {
                        a.push ([ obj[i][0], mkValue(obj[i][1]) ])
                    }
                    return Record.mkRecord(a);
                case Ty.TroupeType.LIST:
                    return mkList(deserializeArray(obj))
                case Ty.TroupeType.TUPLE:
                    return mkTuple(deserializeArray(obj))
                case Ty.TroupeType.CLOSURE:
                    return mkClosure(obj.ClosureID)
                case Ty.TroupeType.NUMBER: 
                case Ty.TroupeType.BOOLEAN:  
                case Ty.TroupeType.STRING:
                    return obj;
                case Ty.TroupeType.PROCESS_ID:
                    return new ProcessID(obj.uuid, obj.pid, obj.node)
                case Ty.TroupeType.AUTHORITY:
                    // 2018-10-18: AA: authority attenuation based on the trust level of the sender 
                    return new Authority(_trustGLB(mkLevel(obj.authorityLevel)))
                case Ty.TroupeType.LEVEL:
                    return mkLevel(obj.lev)
                case Ty.TroupeType.LVAL:
                    return mkValue(obj)
                case Ty.TroupeType.ATOM:
                     return new Atom(obj.atom, obj.creation_uuid)
                case Ty.TroupeType.UNIT:
                    return __unitbase
                default:
                    return obj;
            }
        }

        return new LVal(value(), _trustGLB(lev), _trustGLB(tlev));
    }

    for (let i = 0; i < sercloss.length; i++) {
        mkClosure(i);
    }

    for (let i = 0; i < serenvs.length; i++) {
        mkEnv(i);
    }

    let v = mkValue(serobj.value);

    // go over the namespaces we have generated
    // and load all libraries before calling the last callback

    function loadLib(i: number, cb) {
        if (i < ctxt.namespaces.length) {
            __rtObj.linkLibs(ctxt.namespaces[i]).then(() => loadLib(i + 1, cb))
        } else {
            cb();
        }
    }

    loadLib(0, () => desercb(v));
}

// 2018-11-30: AA: TODO: implement a proper deserialization queue instead of 
// the coarse-grained piggybacking on the event loop

function deserializeCb(lev: Level, jsonObj: any, cb: (body: LVal) => void) {
    if (__isCurrentlyUsingCompiler) {
        setImmediate(deserializeCb, lev, jsonObj, cb) // postpone; 2018-03-04;aa
    } else {
        __isCurrentlyUsingCompiler = true // prevent parallel deserialization attempts; important! -- leads to nasty 
        // race conditions otherwise; 2018-11-30; AA
        __trustLevel = lev;
        __currentCallback = cb;      // obs: this is a global for this module; 
        // the access to it should be carefully controlled

        // we need to share this object with the callbacks

        __currentDeserializedJson = jsonObj; // obs: another global that we must be careful with

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
    }
}

export function deserialize(lev: Level, jsonObj: any): Promise<LVal> {
    return new Promise((resolve, reject) => {
        deserializeCb(lev, jsonObj, (body: LVal) => {
            resolve(body)
        })
    });
}
