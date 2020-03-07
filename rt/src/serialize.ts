"use strict";
const assert = require('assert');
const { spawn } = require('child_process')
import * as Ty from './TroupeTypes'

let compiler = null;

let rtObj = null;

let __unit;

function setRuntimeObj(rt)  {
  rtObj = rt;
  __unit = rt.__unit;
}


function startCompiler() {
  compiler = spawn(process.env.TROUPE + '/bin/troupec', ['--json']);

  compiler.on ('exit', (code, signal)=>{
    // console.error (code, signal);
    process.exit(code);
  });

  compiler.stdout.on('data', (data) => {
    // debuglog ("data in")
    let d = `${data}`
    // debuglog (d)

    accum += d;

    let marker = "/*-----*/\n\n"
    let j = accum.indexOf(marker);

    // debuglog (j)
    if (j >= 0) {
      compilerOutputReady (accum.slice(0,j));
      accum = accum.slice(j + marker.length);
      // debuglog ("remainder", accum)
    }
  });
}

startCompiler();


// --------------------------------------------------


// debugging mechanisms


function debuglog (...s) {
  let spaces = "";
  for (let j = 0; j < indentcounter; j++) {
    spaces = "  " + spaces;
  }

  s.unshift ("DEBUG:" + spaces)
  console.log.apply (null,s)
}

var indentcounter = 0;

function indent () {
  indentcounter ++;
}

function unindent () {
  indentcounter --;
}

// accumulator of communication with the compiler; reset after
// each deserialization; needed because we have no guarantees about
// how the data coming back from the compiler is chunked

let accum = "";

//simple flag to make sure we handle one deserialization at a time
let processing = false;

let deserializationCallback = null;

let deserializationObject = null;

let trustLevel = null;


function deserializationError ()  {
  console.log ("DESERIALIZATION ERROR HANDLING IS NOT IMPLEMENTED")
  process.exit(1);
}

function compilerOutputReady(data) {
  // debuglog (deserializationObject)

  processing = false;

  let serobj = deserializationObject;

  let desercb = deserializationCallback;

  // 1. reconstruct the namespaces
  let snippets = data.split("\n\n");
  let k = 0;


  for (let i = 0; i < serobj.namespaces.length; i++) {
    let ns = serobj.namespaces[i]
    let nsFun = "";
    
    nsFun += "this.libSet = new Set () \n"
    nsFun += "this.libs = [] \n"
    nsFun += "this.addLib = function (lib, decl) " + 
              " { if (!this.libSet.has (lib +'.'+decl)) { " + 
                 " this.libSet.add (lib +'.'+decl); " + 
                 " this.libs.push ({lib:lib, decl:decl})} } \n"
    nsFun += "this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) } \n"

    
    for (let j = 0; j < ns.length; j++) {   
      if (j > 0) {
        nsFun += "\n\n" // looks neater this way
      }
      let snippetJson = JSON.parse (snippets[k++]);
      // console.log (snippetJson.libs);
      // console.log (snippetJson.fname);
      nsFun += snippetJson.code; 
    }

    let NS:any = new Function ('rt',nsFun)
    // console.log (NS.toString());
    ns.fun = new NS(rtObj)
  }

  // 2. reconstruct the closures and environments
  let closures = serobj.closures;

  let envs = serobj.envs;

  function mkClosure (i) {
    if (closures[i].obj) {
      return closures[i].obj;
    } else {
      let nm = serobj.namespaces[closures[i].namespacePtr.NamespaceID].fun;
      let fn = nm [closures[i].fun];
      closures[i].obj = new rtObj.Closure( {}, nm, fn );
      closures[i].obj.env = mkEnv ( closures[i].envptr.EnvID )
      return closures[i].obj;
    }
  }

  function mkEnv (i) {
    if (envs[i].obj) {
      return envs[i].obj;
    } else {
      let env = {} ;
      envs[i].obj = env;
      for (var field in envs[i]) {
        if (field != "obj") { // needed, because otherwise we include the newly created `obj` field;
                              // which leads to a circular dependency down the road at stringification...
                              // 2018-03-05; AA
          env[field] = mkValue (envs[i][field]);
        }
      }
      return envs[i].obj;
    }
  }

  function levRepToLevel ( lev ) {    
    return rtObj.levels.mkLevel (lev);
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

  function mkValue (arg) {
    // debuglog ("*** mkValue", arg);
    assert(Ty.isLVal(arg));
    let obj = arg.val;
    let lev = levRepToLevel (arg.lev);
    let tlev = levRepToLevel (arg.tlev);

    function _trustGLB ( x ) {
      return (rtObj.glb (x, trustLevel))
    }
    
    function value () {
      
      if (Array.isArray (obj)) {
        let a = [];
        for (let i = 0; i < obj.length; i++) {
          a.push(mkValue(obj[i]));
        }  
        let marker;        
        if (arg.tupleKind && arg.tupleKind == true ) {
          marker = rtObj.mkTuple;
        } else {          
          marker = rtObj.mkList;
        }  
        return marker(a)  
      } else if (typeof obj.ClosureID != "undefined") {
        return mkClosure (obj.ClosureID)
      } else if (typeof obj.envptr != "undefined") {
        return mkEnv (typeof obj.envptr.EnvID)
      } else if (typeof(obj) === 'number' || (typeof (obj) ==='boolean')  || typeof (obj) === 'string') {
        return obj 
      } else if ( Ty.isProcessId(obj) ) {
        return new rtObj.ProcessID(obj.uuid, obj.pid, obj.node)
      } else if ( Ty.isAuthority (obj)) {
        // 2018-10-18: AA: authority attenuation based on the trust level of the sender 
        return new rtObj.Authority ( _trustGLB (levRepToLevel (obj.authorityLevel)) )
      } else if (Ty.isLevel(obj)) {      
        return levRepToLevel(obj.lev)
      } else if (Ty.isLVal (obj)) {
        return mkValue (obj)
      } else if (Ty.isAtom (obj) ) {            
        return new rtObj.Atom (obj.atom, obj.creation_uuid) 
      } else if (Object.keys(obj).length == 0) {
        return rtObj.__unitbase
      } else { 
        return obj
        // aa; 2018-03-04; we should in principle have an exhaustive list of values here and
        // do not have such a default
      }  
    }

    return new rtObj.LVal (value(), _trustGLB(lev), _trustGLB (tlev));    
  }

  for (let i = 0; i < closures.length; i++) {
    mkClosure (i);
  }

  for (let i = 0; i < envs.length; i++) {
    mkEnv (i);
  }

  let v = mkValue (serobj.value);

  // go over the namespaces we have generated
  // and load all libraries before calling the last callback

  function loadLib(i, cb) {
    if  (i < serobj.namespaces.length ) {
      serobj.namespaces[i].fun.loadlibs( () => loadLib(i+1, cb));
    } else  {
      cb();
    }
  }

  loadLib ( 0, () => desercb(v));
}



// 2018-11-30: AA: TODO: implement a proper deserialization queue instead of 
// the coarse-grained piggybacking on the event loop

function deserialize(lev, jsonObj, cb) {    
  if (processing) {    
    setImmediate (deserialize, lev, jsonObj, cb) // postpone; 2018-03-04;aa
  } else {
    processing = true // prevent parallel deserialization attempts; important! -- leads to nasty 
                     // race conditions otherwise; 2018-11-30; AA
    trustLevel = lev;
    deserializationCallback = cb;      // obs: this is a global for this module; 
                                     // the access to it should be carefully controlled

    // we need to share this object with the callbacks
    // perhaps reset callbacks?
    // debuglog("* s deserialize", jsonObj);
    let serializedObj = jsonObj;
    // debuglog (serializedObj);
    
    deserializationObject = serializedObj; // obs: another global that we must be careful with

    if (serializedObj.namespaces.length > 0) {
      for (let i = 0; i < serializedObj.namespaces.length; i++) {
        let ns = serializedObj.namespaces[i];
        for (let j = 0; j < ns.length; j++) {
          // debuglog("*s deserialize", ns[j]);          
          compiler.stdin.write( ns[j][1] );
          compiler.stdin.write("\n")
          // debuglog ("data out")
        }
      }
      compiler.stdin.write("!ECHO /*-----*/\n")
    } else {
      // shortcutting the unnecessary interaction with the compiler
      // 2018-09-20: AA
      compilerOutputReady("");
    }    
  }
}

function deserializeAsync (lev, jsonObj) {
  return new Promise ( (resolve, reject) => {
    deserialize (lev, jsonObj, (body) => {
      resolve (body)
    })
  });
}


function stopCompiler() {  
  compiler.stdin.end();
}


/*
..######..########.########..####....###....##.......####.########....###....########.####..#######..##....##
.##....##.##.......##.....##..##....##.##...##........##.......##....##.##......##.....##..##.....##.###...##
.##.......##.......##.....##..##...##...##..##........##......##....##...##.....##.....##..##.....##.####..##
..######..######...########...##..##.....##.##........##.....##....##.....##....##.....##..##.....##.##.##.##
.......##.##.......##...##....##..#########.##........##....##.....#########....##.....##..##.....##.##..####
.##....##.##.......##....##...##..##.....##.##........##...##......##.....##....##.....##..##.....##.##...###
..######..########.##.....##.####.##.....##.########.####.########.##.....##....##....####..#######..##....##
*/


function serialize(x, pclev) {
  let seenNamespaces = new Map();
  let seenClosures   = new Map();
  let seenEnvs       = new Map();

  let namespaces = [];
  let closures   = [];
  let envs       = [];


  let level = pclev;

  function walk (lval)  {    
    assert(Ty.isLVal(lval));

    level =  rtObj.lub (level, lval.lev); // 2018-09-24: AA: is this the only place 
                                       // where we need to check the level of the message?

    let jsonObj;
    let x = lval.val;

    let tupleKind = false;

    let _tt = Ty.getTroupeType(x);

    switch (_tt) {
      case Ty.TroupeType.LIST:
          jsonObj = [];          
          let y = x.toArray()
          for (let i = 0; i < y.length; i ++) {
            jsonObj.push (walk (y[i]));
          }                   
          tupleKind = Ty.isTuple (x)          
          break;
      case Ty.TroupeType.TUPLE:
          jsonObj = [];          
          for (let i = 0; i < x.length; i ++) {
            jsonObj.push (walk (x[i]));
          }                   
          tupleKind = Ty.isTuple (x)          
          break;
      case Ty.TroupeType.CLOSURE:
          if (seenClosures.has(x)) { // debuglog ("pointer to [existing] heap object", seen.get(x))
            jsonObj = { ClosureID : seenClosures.get(x)};
          } else {
            jsonObj  = { ClosureID : closures.length  }
            seenClosures.set(x, closures.length  );
            let jsonClosure:any = {};
            closures.push (jsonClosure);

            let jsonEnvPtr;
            if (seenEnvs.has (x.env)) {
              jsonEnvPtr = { EnvID: seenEnvs.get(x.env)}
            } else {
              jsonEnvPtr = { EnvID: envs.length };
              seenEnvs.set (x.eqnv, envs.length)
              let jsonEnv = {};
              envs.push (jsonEnv);

              for (let field in x.env) {
                if (field != "ret" && field != "_is_rt_env") {
                  let y = x.env[field];
                  jsonEnv[field] = walk (y);
                }
              }
            }

            jsonClosure.envptr = jsonEnvPtr;
            for ( let ff in x.namespace ) {
              if (x.namespace[ff] == x.fun) {
                let jsonNamespacePtr;
                let namespace ;
                if (seenNamespaces.has(x.namespace)) {
                  let n_id = seenNamespaces.get (x.namespace);
                  jsonNamespacePtr = { NamespaceID: n_id };
                  namespace = namespaces[n_id];
                } else {
                  jsonNamespacePtr = { NamespaceID : namespaces.length };
                  seenNamespaces.set ( x.namespace, namespaces.length );
                  namespace = new Map ();
                  namespaces.push (namespace);              
                }

                namespace.set (ff, x.fun.serialized)

                function dfs (deps) {
                  for (let depName of deps) {
                    if (!namespace.has (depName) )  {
                      namespace.set(depName, x.namespace[depName].serialized);
                      dfs (x.namespace[depName].deps);
                    }
                  }
                }

                dfs (x.fun.deps);

                jsonClosure.namespacePtr = jsonNamespacePtr;
                jsonClosure.fun = ff;
              }
            }
          }
          break;
      case Ty.TroupeType.LEVEL:
          jsonObj = { lev : x.stringRep(), isLevel : true };
          break;
      case Ty.TroupeType.LVAL:
          jsonObj = walk (x);
          break;
      case Ty.TroupeType.AUTHORITY:
          jsonObj = { authorityLevel: x.authorityLevel.stringRep() } 
          break;
      case Ty.TroupeType.ATOM:
          jsonObj = { atom: x.atom, creation_uuid: x.creation_uuid };
          break;
      default:
          jsonObj = x;
    }

    // OBS: we are moving away from LVal representation
    // to a more explicit tuple that is different on purpose
    // from LVal. 2018-09-20: AA; We should ideally encapsulate
    // that in a different class with a name that reflects that 
    // this is a transport-level representation. 

    return { val: jsonObj
           , lev : lval.lev.stringRep()
           , tlev: lval.tlev.stringRep()
           , tupleKind : tupleKind
           , troupeType: _tt   // Obs: we cannot use this in deserialization yet, 
                               // because of backward compatibility of the 
                               // runners in the version released for LBS; 2020-03-03; AA
           };              
  }

  let value = walk (x);
  value.lev = rtObj.lub (x.lev, pclev).stringRep () ;


  let nsp = [];
  for (let j = 0; j < namespaces.length; j ++) {
    nsp.push (Array.from(namespaces[j]));
  }

  let serializeObj = { libdeps: []
                     , namespaces: nsp
                     , closures: closures
                     , envs: envs
                     , value: value };

  
  // TODO: propagate the level; 
  return { data: serializeObj, level : level } 
}



module.exports = {
  serialize : serialize,
  deserialize : deserialize,
  deserializeAsync : deserializeAsync,
  stopCompiler: stopCompiler,
  setRuntimeObj: setRuntimeObj  
}
