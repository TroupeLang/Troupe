'use strict'
/*
..######..########.########..####....###....##.......####.########....###....########.####..#######..##....##
.##....##.##.......##.....##..##....##.##...##........##.......##....##.##......##.....##..##.....##.###...##
.##.......##.......##.....##..##...##...##..##........##......##....##...##.....##.....##..##.....##.####..##
..######..######...########...##..##.....##.##........##.....##....##.....##....##.....##..##.....##.##.##.##
.......##.##.......##...##....##..#########.##........##....##.....#########....##.....##..##.....##.##..####
.##....##.##.......##....##...##..##.....##.##........##...##......##.....##....##.....##..##.....##.##...###
..######..########.##.....##.####.##.....##.########.####.########.##.....##....##....####..#######..##....##
*/

import assert from 'assert'
import { lub } from './options.mjs';
import * as Ty from './TroupeTypes.mjs'
import { LVal } from './Lval.mjs';
import { Level } from './Level.mjs';
import { StopThreadError, ThreadError } from './TroupeError.mjs';
import { getRuntimeObject } from './SysState.mjs';

import yargs from 'yargs';
let logLevel = yargs.argv.debug ? 'debug': 'info'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('SRL', logLevel);
const debug = x => logger.debug(x)


export class UnserializableObjectError extends StopThreadError {
    obj: LVal
    get errorMessage ()  {
        return (`Unserializable object: ${this.obj.stringRep()}`);
        // TODO: 2021-06-12: improve error reporting 
        // - indicate what the current type of this object is 
        // - and explain why it is the case that it isn't serializable 
    }

    constructor (obj:LVal) {
        super (getRuntimeObject().$t) ;
        this.obj = obj
    }
}


export function serialize(w:LVal, pclev:Level) {    
    let seenNamespaces = new Map();
    let seenClosures = new Map();
    let seenEnvs = new Map();

    let namespaces = [];
    let closures = [];
    let envs = [];


    let level = pclev;

    function walk(lval:LVal) {
        assert(Ty.isLVal(lval));

        level = lub(level, lval.lev); // 2018-09-24: AA: is this the only place 
        // where we need to check the level of the message?

        let jsonObj;
        let x = lval.val;
        


        let _tt = lval.troupeType

        
        

        switch (_tt) {
            case Ty.TroupeType.RECORD:
                jsonObj = [];
                for (let [k,v] of x.__obj.entries()) {
                    jsonObj.push ([k, walk(v)])
                }
                break;
            case Ty.TroupeType.LIST:
                jsonObj = [];
                let y = x.toArray()
                
                for (let i = 0; i < y.length; i++) {
                    jsonObj.push(walk(y[i]));
                }
                break;
            case Ty.TroupeType.TUPLE:
                jsonObj = [];                                                
                for (let i = 0; i < x.length; i++) {
                    jsonObj.push(walk(x[i]));
                }
                break;
            case Ty.TroupeType.CLOSURE:
                if (!Ty.isSerializableClosure (lval.closureType)) {
                    throw new UnserializableObjectError (lval)
                }

                if (seenClosures.has(x)) { // debuglog ("pointer to [existing] heap object", seen.get(x))
                    jsonObj = { ClosureID: seenClosures.get(x) };
                } else {
                    jsonObj = { ClosureID: closures.length }
                    seenClosures.set(x, closures.length);
                    let jsonClosure: any = {};
                    closures.push(jsonClosure);

                    let jsonEnvPtr;
                    if (seenEnvs.has(x.env)) {
                        jsonEnvPtr = { EnvID: seenEnvs.get(x.env) }
                    } else {
                        jsonEnvPtr = { EnvID: envs.length };
                        seenEnvs.set(x.eqnv, envs.length)
                        let jsonEnv = {};
                        envs.push(jsonEnv);

                        for (let field in x.env) {
                            if (field != "ret" && field != "_is_rt_env" && field != "__dataLevel") {
                                let y = x.env[field];
                                jsonEnv[field] = walk(y);
                            }
                        }
                    }

                    jsonClosure.envptr = jsonEnvPtr;
                    // debug (`the namespace is ${x.namespace}`);
                    for (let ff in x.namespace) {
                        // debug (`the function in the namespace is ${ff.toString()}`)
                        if (x.namespace[ff] == x.fun) {
                            let jsonNamespacePtr;
                            let namespace;
                            if (seenNamespaces.has(x.namespace)) {
                                let n_id = seenNamespaces.get(x.namespace);
                                jsonNamespacePtr = { NamespaceID: n_id };
                                namespace = namespaces[n_id];
                            } else {
                                jsonNamespacePtr = { NamespaceID: namespaces.length };
                                seenNamespaces.set(x.namespace, namespaces.length);
                                namespace = new Map();
                                namespaces.push(namespace);
                            }

                            namespace.set(ff, x.fun.serialized)

                            function dfs(deps) {
                                for (let depName of deps) {
                                    if (!namespace.has(depName)) {
                                        namespace.set(depName, x.namespace[depName].serialized);
                                        dfs(x.namespace[depName].deps);
                                    }
                                }
                            }

                            dfs(x.fun.deps);

                            jsonClosure.namespacePtr = jsonNamespacePtr;
                            jsonClosure.fun = ff;
                        }
                    }
                }
                break;
            case Ty.TroupeType.LEVEL:
                jsonObj = { lev: x.stringRep(), isLevel: true };
                break;
            case Ty.TroupeType.LVAL:
                jsonObj = walk(x);
                break;
            case Ty.TroupeType.AUTHORITY:
                jsonObj = { authorityLevel: x.authorityLevel.stringRep() }
                break;
            case Ty.TroupeType.ATOM:
                jsonObj = { atom: x.atom, creation_uuid: x.creation_uuid };
                break;
            case Ty.TroupeType.LOCALOBJECT: 
                throw new UnserializableObjectError (lval)
            default:
                jsonObj = x;
        }

        // OBS: we are moving away from LVal representation
        // to a more explicit tuple that is different on purpose
        // from LVal. 2018-09-20: AA; We should ideally encapsulate
        // that in a different class with a name that reflects that 
        // this is a transport-level representation. 

        return {
            val: jsonObj
            , lev: lval.lev.stringRep()
            , tlev: lval.tlev.stringRep()
            , troupeType: _tt               
        };
    }

    let value = walk(w);
    value.lev = lub(w.lev, pclev).stringRep();


    let nsp = [];
    for (let j = 0; j < namespaces.length; j++) {
        nsp.push(Array.from(namespaces[j]));
    }

    let serializeObj = {
        libdeps: []
        , namespaces: nsp
        , closures: closures
        , envs: envs
        , value: value
    };

    // TODO: propagate the level; 
    return { data: serializeObj, level: level }
}
