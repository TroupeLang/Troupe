'use strict'
const assert = require('assert');
const request = require('request');
const ThreadError = require('./ThreadError.js').ThreadError
const AggregateError = require('aggregate-error');
const fs = require('fs');
const util = require('util');

const RtClosure = require('./RtClosure.js')

class RtEnv {

  constructor() {
    this._is_rt_env = true;
    // this.ret = __sched.ret;
  }
}




const { isListFlagSet, isTupleFlagSet } = require ('./ValuesUtil.js');


const { promisify } = require ('util')
const readFile = promisify (fs.readFile);

const colors = require ('colors/safe')

const os = require('os');

const uuidv4 = require('uuid/v4');
const rt_uuid = uuidv4();

let yargs = require('yargs');


let logLevel = yargs.argv.debug?'debug':'info';

const logger = require('./logger.js').mkLogger('RTM', logLevel);


const info = x => logger.info(x)
const debug = x => logger.debug(x)
const error = x => logger.error(x)

const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
})

const lineBuffer = [];
const readlineCallbacks = []



function lineListener (input) {  
  if (readlineCallbacks.length > 0 ) {
    let cb = readlineCallbacks.shift ();
    cb (input);
  } else {
    lineBuffer.push (input);
  }
}

readline.on ('line', lineListener)



// an attempt to modularize the runtime; 2018-07-16; AA
//
const Scheduler = require('./Scheduler.js');
const LVal = require('./Lval.js').LVal;
const proc = require('./process.js');

const MailboxProcessor = require('./MailboxProcessor.js');
const NodeManager = require('./NodeManager.js');
const loadLibs = require('./loadLibs.js');
const BaseFunction = require('./BaseFunction.js').BaseFunction;

const SandboxStatus = require('./SandboxStatus.js').HandlerState;


const Authority = require('./Authority.js').Authority;
const options = require('./options.js');

const levels = options;

const Level = require('./Level.js').Level;

const lub = levels.lub;

function lubs (x) {
  if (x.length == 0) {
    return levels.BOT;
  } else {
    let r = x[0];
    for (let i = 1; i < x.length; i++) {
      r = lub (r, x[i]);
    }
    return r;
  }
}

const glb = levels.glb;
const flowsTo = levels.flowsTo;

let ProcessID = proc.ProcessID;
const __unitbase = require('./UnitBase.js');
let SS = require('./serialize.js')
// let WS = require('./webserver.js')

let p2p = require('./p2p/p2p.js')


const __sched = new Scheduler(rt_uuid);
const __theMailbox = new MailboxProcessor(__sched);

let aliases = yargs.argv.aliases
                  ? JSON.parse ( fs.readFileSync(yargs.argv.aliases))
                  : {}





const __nodeManager = new NodeManager(levels, aliases ); // 2019-01-03: todo: use options; AA

let __p2pRunning = false;
// these are initialized later in webServerReady handler
// once we get information from the webserver about the
// port on which we are listening...


/////////////////////////////////////
//
//

// returns the current thread
function $t() { return __sched.__currentThread};

let mkBase                            = (f,name=null) => __sched.mkBase(f, name);
let rt_mkVal                          = (x) => __sched.mkVal(x);
let rt_mkValPos                       = (x, p) => __sched.mkValPos (x, p);
let rt_mkCopy                         = (x) => __sched.mkCopy(x);
let raiseCurrentThreadPC              = (l) => $t().raiseCurrentThreadPC(l);
let raiseCurrentThreadPCToBlockingLev = (l) => $t().raiseCurrentThreadPCToBlockingLev(l);
let raiseCurrentBlockingThreadLev     = (l) => $t().raiseBlockingThreadLev (l);
let currentThreadPid                  = () => __sched.currentThreadId;
const __unit = __sched.__unit;


/////////////////////////////////////




class LibEnv {
  constructor() {
    this.ret = null;
  }
}


let rt_self = mkBase(  (env, arg) => {
  // debug ("* rt self", currentPid);
  rt_ret(currentThreadPid());
}, "self");


// --------------------------------------------------

async function spawnAtNode(nodeid, f) {
  // debug ("* rt spawnAtNode ", nodeid);
  let node = __nodeManager.getNode(nodeid.val);
  // debug ("XX", node);

  // TODO: 2018-09-24: AA: do the information flow check

  let { data, level } = SS.serialize(f, lub(__sched.pc, nodeid.lev));

  let trustLevel = nodeTrustLevel (node.nodeId);

  if (!flowsTo (level, trustLevel)) {
    threadError ("Illegal trust flow when spawning on a remote node\n" + 
                 ` | the trust level of the recepient node: ${trustLevel.stringRep()}\n` + 
                 ` | the level of the information in spawn: ${level.stringRep()}`)
  }


  // 0. we assume that the node is different from
  //    the local node

  // 1. we make a connection to the remote node
  // 2. we send the serialized version of f
  // 3. we wait for the reply (should be a pid)
  // 4. we return the obtained pid
  //--------------------------------------------------


  let theThread = $t();

  try {
    let body1 = await p2p.spawnp2p(node.nodeId, data);
    let body = await SS.deserializeAsync ( nodeTrustLevel(node.nodeId) , body1)
    let pid = new ProcessID(body.val.uuid, body.val.pid, body.val.node);
    theThread.returnInThread (new LVal(pid, body.lev));

    __sched.scheduleThreadT(theThread);
    __sched.resumeLoopAsync();

  } catch (err) {
    error("error spawning remotely; this blocks current thread")
    if (err instanceof AggregateError) {
      for (let ie in err) {
        error (`${ie}`)
      }
    } else {
      error (`${err}`)
    }
  }
}

let _allowRemoteSpawn = false;
function remoteSpawnOK ()  {
  return _allowRemoteSpawn;
}


/**
 *
 * This function is invoked when someone spawns a thread
 * on our node.
 *
 * @param {*} jsonObj
 *    The payload function.
 *
 * @param {*} rtcb
 *    The callback to the networking runtime (e.g., p2p subsystem)
 *    that we invoke with the newly generated process id. This is
 *    needed to communicate the new pid to the spawner.
 *
 * @param {*} fromNode
 *    The identity of the node that initiates the spawning.
 */
async function spawnFromRemote(jsonObj, fromNode) {
  
// 2018-05-17: AA; note that this _only_ uses the lf.lev and
// is completely independent of the current thread's pc;

  let nodeLev = nodeTrustLevel (fromNode);
  
  let lf = await SS.deserializeAsync(nodeLev, jsonObj)
  let f = lf.val;
  let newPid = 
    __sched.scheduleNewThreadAtLevel(      
          f.fun
        , [f.env, __unit]
        , f.namespace
        , lf.lev
        , lf.lev
        );

  // 2018-09-19: AA: because we need to send some info back, we have to invoke
  // serialization.

  let serObj = SS.serialize ( newPid, levels.BOT ).data
  __sched.resumeLoopAsync();
  return (serObj);
}


let rt_sleep = mkBase ((env, arg) => {
    assertIsNumber (arg);
    let delay = arg.val; 
    let theThread = $t();
    theThread.sleeping = true;
    theThread.timeoutObject = 
      setTimeout ( () => {
          __sched.__currentThread = theThread;   // probably unnecessary because we don't create any labeled values here.
          theThread.sleeping = false;
          theThread.timeoutObject = null;
          theThread.returnInThread (__unit);

          __sched.scheduleThreadT(theThread);
          __sched.resumeLoopAsync();
         
      }, delay)
}, "sleep")


function rt_raisedToLev (x, y) {
    return new LVal(x.val, lub(x.lev, y));
}

let rt_sandbox = mkBase ((env, arg) => {
  assertIsNTuple(arg, 2);
  let theThread = $t();
  let threadState = theThread.exportState()  

  
  let done = false;
  let trapperInvoked = false;

  let delay = arg.val[0];
  let retVal = null;
  raiseCurrentThreadPC(delay.lev);

  function mk_tupleVal (x) {
    return theThread.mkVal (rt_mkTuple (x));
  }

  function ok(x, l) {
    let statusOk = $t().mkValWithLev ( true, l  );
    let y = rt_raisedToLev (x, l);
    return mk_tupleVal ( [ statusOk, y ]);    
  } 

  function bad (x, l) {
    let statusBad = __sched.__currentThread.mkValWithLev (false, l) ;
    let y = rt_raisedToLev (x, l);
    return mk_tupleVal ( [ statusBad, y ])
  }

  setTimeout ( () => {            
      theThread.handlerState = new SandboxStatus.NORMAL();
      let resultLabel = __sched.blockingTopLev;

      // Restore the state back to what it was before starting the sandboxing

      theThread.importState (threadState);
     
      // __sched.raiseCurrentThreadPCToBlockingLev();

      // 2019-01-31: AA; obs: this is subtle

      // we check whether the thread is no longer scheduled
      if (done || trapperInvoked || theThread.sleeping ) {
          if (done) {
              theThread.returnInThread(ok(retVal, resultLabel));
          } else {
              if (theThread.sleeping)  {
                  theThread.sleeping = false;
                  clearTimeout (theThread.timeoutObject);
              }
              theThread.returnInThread (bad (__unit, resultLabel));
          } 
          
          // because the thread has finished, we need 
          // to push it back into the thread pool

          __sched.scheduleThreadT(theThread);
          __sched.resumeLoopAsync();

      } else {
          theThread.killCounter++;
          // the thread is alive and is somewhere in the scheduler queue, so
          // we just change its return kont
          theThread.returnInThread ( bad (__unit, resultLabel ));
      }
    }, delay.val) 


  /*
  let barrierClosure = new RtClosure ({ret:null}, null, (env, arg) => {  
    retVal = arg;
    done = true;
  });
  */

  let guard = (arg) => {
    retVal = arg;
    done = true;
  }


  let trapper = mkBase ((env, arg) => {
    trapperInvoked = true;
    retVal = __unit;
  })

  // __sched.setret (barrierClosure);
  $t().callInThread(guard);
  theThread.handlerState = new SandboxStatus.INSANDBOX(trapper);
  theThread.barrierdepth = 0;
  rt_tailcall (arg.val[1], __unit);

}, "sandbox")


let rt_spawn = mkBase((env, larg) => {
  assertNormalState("spawn")
  // debug ("* rt rt_spawn *", larg.val, larg.lev);
  raiseCurrentThreadPC(larg.lev);
  let arg = larg.val;

  function spawnLocal(arg) {
    // debug ("scheduled rt_spawn ", arg.fun);

    let newPid = __sched.scheduleNewThreadAtLevel (
                      arg.fun, 
                      [arg.env, __unit], 
                      arg.namespace, 
                      __sched.pc, 
                      __sched.blockingTopLev)
    rt_ret(newPid);
  }


  if (Array.isArray(arg)) {    
    if (__nodeManager.isLocalNode(arg[0].val)) { // check if we are at the same node or note
      // debug ("SAME NODE")
      raiseCurrentThreadPC(lub(arg[0].lev, arg[1].lev));
      assertIsFunction(arg[1]);
      spawnLocal(arg[1].val)
    } else {
      assertIsNode (arg[0]);
      assertIsFunction (arg[1]);
      return spawnAtNode(arg[0], arg[1])
    }
  } else {
    assertIsFunction (larg);
    spawnLocal(arg)
  }
}, "spawn");


function persist (obj, path) {
  let jsonObj = SS.serialize(obj, __sched.pc).data;
  fs.writeFileSync(path, JSON.stringify(jsonObj));  
}

let rt_save = mkBase((env, larg) => {
  assertIsNTuple(larg, 2);
  raiseCurrentThreadPC(larg.lev);
  let arg = larg.val;
  let file = arg[0].val;
  let data = arg[1];
  persist (data, "./out/saved." + file + ".json" )
  rt_ret(__unit);
}, "save")


let rt_restore = mkBase((env, arg) => {
  assertIsString(arg)
  let theThread = $t();
  let file = arg;

  (async () => {
    let jsonStr = await fs.promises.readFile("./out/saved." + file.val + ".json");
    let data = await SS.deserializeAsync(levels.TOP, JSON.parse(jsonStr));
    theThread.returnInThread(data);
    __sched.scheduleThreadT(theThread);
    __sched.resumeLoopAsync();

  }) ()
}, "restore")


/**
 * This function is called when someone sends us a message.
 *
 * @param {*} pid
 *    The process id of the sender
 * @param {*} jsonObj
 *    The payload
 * @param {*} fromNode
 *    The node identity of the sender node
 */
async function receiveFromRemote(pid, jsonObj, fromNode) {
  let data = await SS.deserializeAsync ( nodeTrustLevel(fromNode)  , jsonObj)
  // debug ("* rt receiveFromremote * " + fromNode);

  // TODO: 2018-07-23: do we need to do some more raising
  // about the level of the fromNode?; AA

  let fromNodeId = __sched.mkVal(fromNode);
  let toPid = new LVal(new ProcessID(rt_uuid, pid, __nodeManager.getLocalNode()), data.lev);
  __theMailbox.addMessage(fromNodeId, toPid, data.val, data.lev);
  __sched.resumeLoopAsync();

}


/**
 * Sends the provided mesasge to a remote process, first doing the information
 * flow check that the remote process is not going to violate our trust
 * assumptions.
 *
 * @param {*} toPid   The pid of the remote process
 * @param {*} message The data to send
 *
 */
function sendMessageToRemote(toPid, message) {
  let node = toPid.node.nodeId;
  let pid = toPid.pid;
  // debug ("* rt *", toPid, message);

  let {data, level} = SS.serialize(new LVal(message, __sched.pc), __sched.pc);
  let trustLevel = nodeTrustLevel (node);

  // debug ("data level: " +  level.stringRep());
  // debug ("remote trust level: " + trustLevel.stringRep());

  if (!flowsTo (level, trustLevel)) {
    threadError ("Illegal trust flow when sending information to a remote node\n" + 
                 ` | the trust level of the recepient node: ${trustLevel.stringRep()}\n` + 
                 ` | the level of the information to send:  ${level.stringRep()}`);
  } else {
    p2p.sendp2p(node, pid, data)
    rt_ret(__unit); // we return unit to the call site at the thread level
  }
}


function isLocalPid(pid) {
  let x = pid.uuid.toString() == rt_uuid.toString();
  return (x);
}

// TODO: 2020-02-08; consider refactoring this to the Thread module; AA
function rt_mkuuid () {
  let pid = uuidv4();
  let uuidval = rt_mkVal ( pid );
  return uuidval;  
}  

function rt_sendMessageNochecks (lRecipientPid, message, ret=true) {
  
  let recipientPid = lRecipientPid.val;

  if (isLocalPid(recipientPid)) {        
    let nodeId = __sched.mkVal(__nodeManager.getNodeId());
    __theMailbox.addMessage(nodeId, lRecipientPid, message, __sched.pc);    

    if (ret) {      
      rt_ret( __unit);
    } 
  } else {
    // debug ("* rt rt_send remote *", recipientPid, message);
    sendMessageToRemote(recipientPid, message)
  }
}

let rt_send = mkBase((env, larg) => {
  raiseCurrentThreadPCToBlockingLev();
  assertNormalState("send")
  raiseCurrentThreadPC(larg.lev);
  assertIsNTuple(larg, 2);
  assertIsProcessId(larg.val[0]);
  let arg = larg.val;
  // we need to check whether the recipient process is local
  // if yes, then we just proceed by adding the message to the
  // local mailbox; otherwise we need to proceed to serialization
  // external call.

  let lRecipientPid = arg[0];
  // debug ("* rt rt_send *", lRecipientPid);
  raiseCurrentThreadPC(lRecipientPid.lev); // this feels a bit odd.
  let message = arg[1];

  rt_sendMessageNochecks(lRecipientPid, message)
  
}, "send");


 
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


// this function must only be called from 
// one of the checked functions 
function _receiveFromMailbox (lowb, highb, handlers) {

  let mclear = $t().mailbox.mclear
  
  let is_sufficient_clearance = 
    flowsTo( lub (highb.val, __sched.pc)
          ,  lub (lowb.val, mclear.boost_level ))

    if (!is_sufficient_clearance)  {  
      let errorMessage = 
        "Not enough mailbox clearance for this receive\n" +
        ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
        ` | receive upper bound: ${highb.val.stringRep()}\n` +
        ` | pc level           : ${__sched.pc.stringRep()}\n` +
        ` | mailbox clearance  : ${mclear.boost_level.stringRep()}` 
      threadError (errorMessage);
    }    
  
    let is_clearance_a_leak = flowsTo( mclear.pc_at_creation, glb (__sched.pc, lowb.val))

    if (!is_clearance_a_leak)  {
      let errorMessage = 
        "PC level at the time of raising the mailbox clearance is too sensitive for this receive\n" +
        ` | receive lower bound: ${lowb.val.stringRep()}\n` + 
        ` | pc level at the time of receive: ${__sched.pc.stringRep()}\n` +        
        ` | pc level at the time of raise: ${mclear.pc_at_creation.stringRep()}`  // we need better terminology for these       
      threadError (errorMessage);
    }

    __theMailbox.rcv(lowb.val, highb.val, handlers);    
  
}


function receiveBoundedRangeWithAuthority(env, arg ) {
  assertNormalState ("rcv");
  assertIsNTuple(arg, 3);
  assertIsLevel (arg.val[0])
  assertIsLevel (arg.val[1])  
  assertIsList (arg.val[2])
  let lowb = arg.val[0]
  let highb = arg.val[1]  
  let handlers = arg.val[2]
  _receiveFromMailbox (lowb, highb, handlers);
}

function receiveAtOneLevel(env, arg) {
  assertNormalState ("rcvp")
  assertIsNTuple (arg, 2)
  assertIsLevel (arg.val[0])
  assertIsList (arg.val[1])
  let lev = arg.val[0]
  let handlers = arg.val[1];
  _receiveFromMailbox (lev, lev, handlers)
}


function baseRcv(env, handlers) {
  assertNormalState("receive")
  assertIsList (handlers)
  // shortcutting level checks because they are guaranteed 
  // to hold when both low and upper bound is pc; 2020-02-08; AA
  __theMailbox.rcv(__sched.pc, __sched.pc, handlers);
}


let rt_receive = mkBase(baseRcv)
let rt_rcvp = mkBase(receiveAtOneLevel)
let rt_rcv = mkBase(receiveBoundedRangeWithAuthority)


function formatToN ( s, n )  {
  if (s.length < n) {
    let j = s.length;
    for (; j < n; j ++) {
      s = s + " ";
    }
  }
  return s;
}

let rt_exit = mkBase ((env,arg) =>  {
  assertNormalState ("exit");
  assertIsNTuple(arg, 2);
  assertIsAuthority(arg.val[0]);
  assertIsNumber(arg.val[1]);
  assertIsTopAuthority(arg.val[0]);
  cleanup();
  process.exit(arg.val[1].val); 
},"exit")


let rt_getTime = mkBase ((env,arg)=>{
  assertIsUnit(arg)
  let d = new Date()
  let t = d.getTime()
  let v = new LVal (t, __sched.pc);
  rt_ret (v)
} )

let rt_printWithLabels = mkBase((env, arg) => {
  console.log (
        $t().mkCopy(arg).stringRep(false)
        );
  
  rt_ret(__unit);
}, "printWithLabels");


let rt_toString = mkBase ((env, arg)=> {

  let taintRef = {lev : __sched.pc};
  let s = $t().mkCopy(arg).stringRep 
                        (true,  // omit labels
                         taintRef  // accumulate taint into this reference
                        )

  let r = $t().mkValWithLev (s, taintRef.lev) ;  
  rt_ret (r);
}, "toString")


let rt_toStringLabeled = mkBase ((env, arg) => {
  let v = $t().mkCopy (arg);
  let taintRef = {lev : __sched.pc};
  
  let s = v.stringRep (false,  // do not omit labels 
                         taintRef  // accumulate taint into this reference
                        )

                
  
  let r = $t().mkValWithLev (s, taintRef.lev) ;  

  
  rt_ret (r);
}, "toStringLabeled")



let rt_print = mkBase((env, arg) => {
  console.log (
        // colors.green (formatToN ( "PID:" +  __sched.currentThreadId.stringRep(), 30)),
        // colors.green (formatToN ( "PC:" +  __sched.pc.stringRep(), 20)),
        // colors.green (formatToN ( "BL:" +  __sched.blockingTopLev.stringRep(), 20)),
        arg.stringRep(true)
        );
    
  rt_ret(__unit);
}, "print");


let rt_printString = mkBase ((env, arg) => {
  assertIsString (arg);
  console.log (arg.val)
  rt_ret (__unit);
}, "printString")

let rt_writeString = mkBase ((env, arg) => {
  assertIsString (arg);
  process.stdout.write (arg.val)
  rt_ret (__unit);
}, "writeString")


let rt_question = mkBase ((env, arg) => {
  readline.removeListener ('line', lineListener);
  let theThread = $t();

  assertIsString (arg);
  theThread.raiseBlockingThreadLev (levels.TOP)

  readline.question (arg.val, (s) => {
    let r = theThread.mkValWithLev (s, levels.TOP)
    theThread.returnInThread (r)
    __sched.scheduleThreadT(theThread);
    __sched.resumeLoopAsync()

    readline.on ('line', lineListener)

  })

}, "question")

let rt_inputline = mkBase ((env, arg) => {
  assertIsUnit (arg)

  let theThread = $t();
  theThread.raiseBlockingThreadLev (levels.TOP)

  

  if (lineBuffer.length > 0 ) {
     let s = lineBuffer.shift();
     let r = theThread.mkValWithLev (s, levels.TOP);
     
     rt_ret (r);
  } else {
    readlineCallbacks.push( (s) => {

      let r = theThread.mkValWithLev (s, levels.TOP)
      theThread.returnInThread (r)
      __sched.scheduleThreadT(theThread);
      __sched.resumeLoopAsync()
    })
  }
}, "inputLine")




let rt_debug = function (s) {
  
  let tid = $t().tidErrorStringRep()
  let pc = __sched.pc.stringRep()
  let bl = __sched.blockingTopLev.stringRep()
  let handler_state = __sched.handlerState.toString()
  console.log(
    colors.red (formatToN ( "PID:" + tid, 50)),
    colors.red (formatToN ( "PC:" +  pc, 20)),
    colors.red (formatToN ( "BL:" +  bl, 20)),
    colors.red (formatToN ( "HN" + handler_state, 20)),
    s
  );
}

let rt_attenuate = mkBase ((env, arg) => {
  assertIsNTuple(arg, 2);
  let argv = arg.val;
  let authFrom = argv[0];
  assertIsAuthority(authFrom);
  let levTo = argv[1];
  assertIsLevel (levTo);

  let ok_to_attenuate = flowsTo (levTo.val, authFrom.val.authorityLevel);

  // todo: 2018-10-18: AA; are we missing anything?
  let l_meta = lubs ( [  __sched.pc, arg.lev, authFrom.lev, levTo.lev] )
  let l_auth = ok_to_attenuate? levTo.val: levels.BOT;
  let r = new LVal ( new Authority (l_auth), l_meta)

  rt_ret ( r)
}, "attenuate")



let rt_declassify = mkBase ((env, arg) => {
//  assertDeclassificationAllowed()// 2019-03-06: AA: allowing declassification everywhere?
  assertIsNTuple (arg, 3);

  let argv = arg.val;
  let data = argv [0];
  
  let auth = argv[1];
  assertIsAuthority (auth);

  let toLevV = argv [2];
  assertIsLevel (toLevV);

  let pc = __sched.pc;

  let levFrom = data.lev;

  // check that levFrom ⊑ auth ⊔ levTo
  let _l = lubs ([auth.val.authorityLevel, toLevV.val]);


  let ok_to_declassify =
    flowsTo (levFrom, _l )

  if (ok_to_declassify) {
    // we need to collect all the restrictions
    let r =  new LVal (data.val, lubs ([toLevV.val, toLevV.lev, pc, arg.lev, auth.lev]));
    rt_ret (r) // schedule the return value
  } else {
    let errorMessage = 
     "Not enough authority for declassification\n" + 
     ` | level of the data: ${data.lev.stringRep()}\n` + 
     ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n` + 
     ` | target level of the declassification: ${toLevV.val.stringRep()}`
    threadError (errorMessage) ;

   // return; // nothing scheduled; should be unreachabele
  }

}, "declassify")

let rt_raiseTrust = mkBase ((env, arg) => {  
  assertNormalState("raise trust");
  assertIsNTuple(arg,3)

  let argv = arg.val;
  let data = argv[0];
  assertIsString (data);
  
  let authFrom = argv[1];
  assertIsAuthority (authFrom);  
  assertIsTopAuthority(authFrom); // AA; 2019-03-07: may be a bit pessimistic, but okay for now
  let levTo = argv[2];
  assertIsLevel (levTo);

  let ok_to_raise = 
    flowsTo (__sched.blockingTopLev, levels.BOT );
  if (!ok_to_raise) {
    threadError ("Cannot raise trust level when the process is tainted\s" + 
                ` | blocking label: ${__nodeManager.blockingTopLev}`)
  }


    //flowsTo (levTo.val, authFrom.val.authorityLevel);
  // AA, 2018-10-20 : beware that no information flow is enforced here
  // let l_meta = lubs ([__sched.pc, arg.lev, authFrom.lev, levTo.lev])
  let l_raise = ok_to_raise ? levTo.val : levels.BOT ;
  let nodeId = __nodeManager.getNode(data.val).nodeId;
  // let nodeId = data.val;
  let currentLevel = nodeTrustLevel (nodeId)
  _trustMap [nodeId] = lub (currentLevel, l_raise);
  rt_ret (__unit);
}, "raiseTrust")


/**
 * Returns a string corresponding to the node identify
 * from a process
 */
let rt_nodeFromProcess = mkBase ((env, arg) => {
  assertIsProcessId (arg);
  let data = arg.val;
  let nodeId = data.node.nodeId;
  let v = new LVal  (nodeId, arg.lev);
  rt_ret (v);
}, "node")


// TODO: check that the arguments to the register are actually pids

let __theRegister = {}
let rt_register = mkBase((env, arg) => {
  assertNormalState("register")
  assertIsNTuple(arg, 3);
  assertIsString ( arg.val[0] )
  assertIsProcessId ( arg.val[1]);

  assertIsAuthority (arg.val[2]);
  assertIsTopAuthority (arg.val[2]);

  let ok_to_raise = 
    flowsTo (__sched.blockingTopLev, levels.BOT );
  if (!ok_to_raise) {
    threadError ("Cannot raise trust level when the process is tainted\s" + 
                ` | blocking label: ${__nodeManager.blockingTopLev}`)
  }


  // TODO: 2018-07-29: info flow checks
  // this is needed, because registration
  // is stateful

  let k = arg.val[0].val;
  let v = arg.val[1];

  __theRegister[k] = v;
  rt_ret(__unit);
}, "register")

async function whereisFromRemote(k) {
  __sched.resumeLoopAsync()
  // TODO: 2018-10-20: make use of the levels as they were
  // recorded during the registration (instead of the bottom here )
  if (__theRegister[k]) {
    let serObj = SS.serialize (__theRegister[k], levels.BOT).data
    return serObj
  }  
}


let rt_whereis = mkBase((env, arg) => {
  assertNormalState("whereis")
  
  assertIsNTuple (arg, 2);
  assertIsNode (arg.val[0]);
  assertIsString (arg.val[1]);
  raiseCurrentBlockingThreadLev(arg.val[0].lev);
  raiseCurrentBlockingThreadLev(arg.val[1].lev);
  

  // let n = dealias(arg.val[0].val);    
  let n = __nodeManager.getNode(arg.val[0].val).nodeId;
  
  let k = arg.val[1].val;
    
  let nodeLev = nodeTrustLevel (n);
  let theThread = $t();
  
  let okToLookup = flowsTo ( lubs ([__sched.pc, arg.val[0].lev, arg.val[1].lev]), nodeLev);
  if (!okToLookup) {
    threadError ("Information flow violation in whereis");
    return;
  }

  if (__nodeManager.isLocalNode(n)) {      
    if (__theRegister[k]) {
      rt_ret( theThread.mkVal (__theRegister[k]) )
    }
  } else {    
    (async () => {     
        try {
          let body1 = await p2p.whereisp2p (n, k);
          let body = await SS.deserializeAsync ( nodeTrustLevel(n), body1 );
          let pid = new ProcessID(body.val.uuid, body.val.pid, body.val.node);

          theThread.returnInThread (theThread.mkValWithLev(pid, body.lev));
          __sched.scheduleThreadT(theThread);
          __sched.resumeLoopAsync();    
          
        } catch (err) {
          debug ("whereis error: " + err.toString())
          throw err;
        }
      
    }) ()
  }
}, "whereis")



let baseMkSecret = function (env, x) {
  // debug ("making secret " + x.val)
  rt_ret(new LVal(x.val, levels.TOP))
}

let rt_mkSecret = mkBase(baseMkSecret)

let baseDisclose = function (env, x) {
  assertNormalState("baseDisclose");
  // assert that
  // pc ⊔ x.lev ⊑ LOW
  
  if (!flowsTo(lub(__sched.joinedLev, x.lev), levels.BOT)) {
    threadError ("Illegal flow in adv function:\n"  + 
                 ` |    pc: ${__sched.pc.stringRep()}\n` + 
                 ` | block: ${__sched.blockingTopLev.stringRep()}\n` +
                 ` | value: ${x.stringRep()}`)
  }
  rt_ret(__unit);
}

let rt_adv = mkBase(baseDisclose)

// --------------------------------------------------
function rt_setret(namespace, kf, e) {
  // assertIsEnv(e);
  let r = new RtClosure(e, namespace, kf);
  __sched.setret(r);
}

function rt_mkLabel(x) {
  // debug ("mkLabel", x, x === "secret");

  return new LVal(levels.mkLevel(x), __sched.pc);

}


function listStringRep(x, omitLevels = false, taintRef = null) {
  if (x.length == 0) {
    return "";
  }
  let s = x[0].stringRep(omitLevels, taintRef);

  for (let i = 1; i < x.length; i++) {
    s += ", " + x[i].stringRep(omitLevels, taintRef );
  }
  return s;
}

function rt_mkTuple(x) {
  x.stringRep = function (omitLevels = false, taintRef  = null) {
    return ("(" + listStringRep(x, omitLevels, taintRef) + ")")
  }
  x.isTuple = true;
  return x;
}

function rt_mkList(x) {
  x.stringRep = function (omitLevels = false, taintRef = null) {
    return ("[" + listStringRep(x, omitLevels, taintRef) + "]")
  }
  x.isList = true;
  return x;
}


function threadError (s, internal = false) {
  return $t().threadError(s,internal);
}

let rt_threadError = threadError;

function rt_error(x) {
  threadError(x.val);
}

function rt_errorPos (x, pos) {
  if (pos != '') {
    threadError(x.val + " at " + pos);
  } else {
    threadError(x.val);
  }
}

function rt_tailcall(lff, arg) {
  assertIsFunction(lff);
  if (!lff.val.fun) {
    console.log ("UNDEF FUN")
  }
  raiseCurrentThreadPC(lff.lev);
  let ff = lff.val;
  //__sched.tailNext ( () => {  ff.fun.apply (ff.namespace, [ff.env, arg]) } );
  // __sched.tailNext ( () => { ff.fun (ff.env, arg) } );
  __sched.tail(ff.fun, ff.env, arg, ff.namespace);
  
}

let rt_ret = (arg) => __sched.returnInThread(arg);


function runtimeEquals(o1, o2) {
  if (typeof o1.atom != "undefined" && typeof o2.atom != "undefined") {
    // obs: atoms operate in a global namespace; 2018-03-09; aa
    return (o1.atom == o2.atom)
  }
  
  
  if (typeof o1.pid != "undefined" &&
    typeof o2.pid != "undefined") { 
    return (proc.pid_val_equals(o1, o2));
  } else {
    return (o1 == o2)
  }
}


function rt_loadLib(lib, decl, obj) {
  // load the lib from the linked data structure
  let r = obj.libs[lib + "." + decl];  
  let rv = rt_mkVal(r);
  // rt_debug("loading lib " + decl);
  return rv;
}


function rt_linkLibs(libs, obj, cb) {
  obj.libs = {}
  loadLibs.loadLibsAsync(libs, obj, cb, rtObj);
}

let _trustMap = {}

function nodeTrustLevel (nodeid) {
  if (_trustMap ) {
    return _trustMap[nodeid]? _trustMap [nodeid] : levels.BOT;
  }
  return levels.BOT;
}

// 2018-12-07: adding some code for enforcing semantics of numbers
// 

let assertIsBoolean = (x) => __sched.__currentThread._asserts.assertIsBoolean(x)
let assertIsNumber = (x) => __sched.__currentThread._asserts.assertIsNumber (x)
let assertIsFunction = (x, internal=false) => __sched.__currentThread._asserts.assertIsFunction(x,internal)
let assertIsHandler = (x) => __sched.__currentThread._asserts.assertIsHandler(x) 
let assertIsUnit = (x) => __sched.__currentThread._asserts.assertIsUnit(x) 
let assertIsListOrTuple = (x) => __sched.__currentThread._asserts.assertIsListOrTuple (x)
let assertIsList = x => __sched.__currentThread._asserts.assertIsList (x)
let assertIsNTuple = (x,n) => __sched.__currentThread._asserts.assertIsNTuple(x,n)
let assertIsString = s => __sched.__currentThread._asserts.assertIsString (s)
let assertIsNode = x => __sched.__currentThread._asserts.assertIsNode (x)
let assertIsProcessId = (x) => __sched.__currentThread._asserts.assertIsProcessId (x) 
let assertIsLevel = (x) => __sched.__currentThread._asserts.assertIsLevel (x) 
let assertIsTopAuthority = (x) => __sched.__currentThread._asserts.assertIsTopAuthority (x) 
let assertIsAuthority = (x) => __sched.__currentThread._asserts.assertIsAuthority (x)
let assertNormalState = (s) => __sched.__currentThread._asserts.assertNormalState (s) 
let assertPairAreNumbers = (x,y) => __sched.__currentThread._asserts.assertPairAreNumbers(x,y)
let assertPairAreStringsOrNumbers = (x,y) => __sched.__currentThread._asserts.assertPairAreStringsOrNumbers(x,y)
let assertIsCapability = (x) => $t()._asserts.assertIsCapability(x)

function RuntimeObject() {
  this.Atom = function (name, creation_uuid = rt_uuid) {
    let atm = {
      atom: name,
      creation_uuid: creation_uuid,
      stringRep: function (omitLevels = false) {
        return name
      }
    }
    return atm
  }
  this.__nodeManager = __nodeManager;
  this.assertIsHandler = assertIsHandler
  this.assertIsNTuple = assertIsNTuple
  this.assertIsFunction = assertIsFunction
  this.Authority = Authority;
  this.ProcessID = ProcessID;
  this.LVal = LVal;
  this.lub = lub;
  this.glb = glb;
  this.error = rt_error;
  this.errorPos = rt_errorPos;
  this.threadError = rt_threadError;
  this.Closure = RtClosure;
  this.Env = RtEnv;
  this.setret = rt_setret
  

  // this.resetret = rt_resetret
  this.ret = rt_ret
  this.__unitbase = __unitbase
  this.__unit = __unit
  this.rt_uuid = __sched.rt_uuid
  this.tailcall = rt_tailcall
  this.mkVal = rt_mkVal
  this.mkValPos = rt_mkValPos
  this.mkCopy = rt_mkCopy
  this.mkTuple = rt_mkTuple
  this.mkList = rt_mkList

  this.loadLib = rt_loadLib

  this.debug = rt_debug 

  this.linkLibs = rt_linkLibs

  this.levels = levels

  this.persist = persist

  this.mkLabel = rt_mkLabel
  this.raisedTo = function (x, y) {
    return new LVal(x.val, lub(lub(x.lev, y.val), y.lev), lubs([x.tlev, y.tlev, __sched.pc]) )
  }
 
  this.unaryMinus = function (x) {
    assertIsNumber(x);
    return new LVal(-x.val, x.lev, x.tlev)
  }

  this.node = rt_nodeFromProcess;
  this.raiseTrust = rt_raiseTrust;
  this.attenuate = rt_attenuate;
  this.declassify = rt_declassify;
  this.toStringL = rt_toStringLabeled;
  this.toString  = rt_toString;
  this.getTime = rt_getTime;
  this.print = rt_print;
  this.printWithLabels = rt_printWithLabels;
  this.printString = rt_printString
  this.writeString = rt_writeString;
  this.inputLine = rt_inputline;
  this.question = rt_question;
  this.self = rt_self;
  this.send = rt_send;
  this.spawn = rt_spawn;
  this.sleep = rt_sleep;
  this.sandbox = rt_sandbox; 
  this.restore = rt_restore;
  this.save = rt_save;
  this.receive = rt_receive;
  this.rcvp = rt_rcvp;
  this.rcv = rt_rcv;
  this.mkSecret = rt_mkSecret;
  this.adv = rt_adv;
  this.register = rt_register;
  this.whereis = rt_whereis;
  this.exit = rt_exit;
  



  this.debugpc = mkBase ((env,arg)=>{
    rt_debug("");
    rt_ret(__unit);
  })

  this.eq = function (x, y) {
    return new LVal(runtimeEquals(x.val, y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }

  this.neq = function (x, y) {
    return new LVal(!(runtimeEquals(x.val, y.val)), lub(x.lev, y.lev), lub (x.tlev, y.tlev))
  }

  this.stringConcat = function (x,y ) {
    assertIsString (x);
    assertIsString (y);
    return new LVal ( (x.val + y.val), lub (x.lev, y.lev), lub (x.tlev, y.tlev));
  }

  this.plus = function (x, y) {
    assertPairAreStringsOrNumbers(x,y);
    return new LVal((x.val + y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev))
  }
  this.minus = function (x, y) {
    assertPairAreNumbers(x,y);
    let rv = new LVal((x.val - y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )    
    return rv; 
  }
  this.mult = function (x, y) {
    assertPairAreNumbers(x,y);
    return new LVal((x.val * y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.div = function (x, y) {
    assertPairAreNumbers(x,y);
    return new LVal((x.val / y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.le = function (x, y) {
    assertPairAreStringsOrNumbers(x,y);
    return new LVal((x.val <= y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.lt = function (x, y) {
    assertPairAreStringsOrNumbers(x,y);
    return new LVal((x.val < y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.ge = function (x, y) {
    assertPairAreStringsOrNumbers(x,y);
    return new LVal((x.val >= y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.gt = function (x, y) {
    assertPairAreStringsOrNumbers(x,y);
    return new LVal((x.val > y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.and = function (x, y) {
    assertIsBoolean(x);
    assertIsBoolean(y);
    return new LVal((x.val && y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.or = function (x, y) {
    assertIsBoolean(x);
    assertIsBoolean(y);
    return new LVal((x.val || y.val), lub(x.lev, y.lev), lub (x.tlev, y.tlev) )
  }
  this.index = function (x, y) {
    assertIsListOrTuple(x);
    assertIsNumber(y);    
    let z = x.val[y.val];    
    return new LVal(z.val, lub(lub(x.lev, y.lev), z.lev), lubs ([x.tlev,y.tlev,z.tlev]) )
  }
  this.islist = function (x) {
    return new LVal(Array.isArray(x.val) && isListFlagSet(x.val), x.lev, x.tlev )
  }
  this.istuple = function (x) {
    return new LVal(Array.isArray(x.val) && isTupleFlagSet(x.val), x.lev, x.tlev )
  }
  this.cons = function (a, b) {
    assertIsList(b) // 2019-03-07: AA; consider forcing the elements of the list to be of the same type (unless nil)
    let x = b.val.slice();
    x.unshift(a);
    return new LVal(rt_mkList(x), b.lev, b.tlev )
  }
  this.length = function (x) {
    assertIsListOrTuple(x);
    return new LVal(x.val.length, x.lev, x.tlev )
  }

  this.head = function (x) {
    assertIsList(x)
    let y = x.val[0];
    return new LVal(y.val, lub(y.lev, x.lev), x.tlev )
  }

  this.tail = function (x) {
    assertIsList (x)
    let y = x.val.slice(1);
    return new LVal(rt_mkList(y), x.lev, x.tlev )
  }

  this.getVal = function (x) {
    return x.val
  }
  this.branch = function (x) {
    $t().setBranchFlag()
    raiseCurrentThreadPC(x.lev);
   
  }

  this.push = (x) => {
    __sched.__currentThread.callInThread (x);
  }

  this.assertOrError = function (x)  {
    raiseCurrentBlockingThreadLev (x.lev);
  }


  this.levelOf = mkBase ((env, arg) => {
    let l = arg.lev; 
    rt_ret (new LVal (l, lub (__sched.pc, l)))
  })


  this.flowsTo = mkBase ((env, arg) => {
    assertIsNTuple(arg, 2);
    let x = arg.val[0];
    let y = arg.val[1];

    assertIsLevel(x);
    assertIsLevel(y);

    rt_ret( new LVal(flowsTo(x.val, y.val), lub (__sched.pc, lub(x.lev, y.lev))))
  })


  this.pcpush = mkBase ((env,arg) => {
    assertNormalState("pcpush")
    assertIsAuthority(arg);
    $t().pcpinipush(arg, "pcpush");
  })


  this.pcpop = mkBase ((env,arg) => {
    assertNormalState("pcpop");
    assertIsCapability (arg);
    $t().pcpop(arg)    
  })


  this.pinipush = mkBase ((env, arg) => {
    assertNormalState("pinipush");
    assertIsAuthority(arg);    
    $t().pcpinipush(arg, "pinipush");    
  })


  this.pinipop = mkBase (( env, arg) => {   
    assertNormalState("pinipop");
    assertIsCapability(arg)
    $t().pinipop(arg);
  })

  /* Implementation note: 2019-01-02; AA: exit capabilities are implemented as
   * records of two functions (see MailboxProcessor.js) -- one for when the
   * handler pattern is successful, and one for when it is not. All we do in the
   * corresponding runtime functions is dynamically check the types, raise the
   * pc levels depending on the arguments, and then just call the corresponding
   * functions. The functions themselves are defined in MailboxProcessor.js.
   */ 
  

  this.setLibloadMode = () => {
    this.mkVal = (x) => new LVal (x, levels.BOT );
    this.mkValPos = (x, pos) => new LVal(x, levels.BOT, levels.BOT, pos );
    this.Env = LibEnv; 
  }

  this.setNormalMode = () => {
    this.mkVal = rt_mkVal;
    this.Env = RtEnv;
  }


  this.mkuuid = mkBase ((env,arg) => {
    assertIsUnit (arg);
    rt_ret (rt_mkuuid()); 
  });

  this.newlabel = mkBase ((env, arg) => {
    assertIsUnit(arg);
    let levid = uuidv4().toString()
    rt_ret (rt_mkLabel(levid));
  })

  this.sendMessageNoChecks = rt_sendMessageNochecks;

  this.raisembox = mkBase ((env,arg) => {
    assertIsLevel (arg);
    $t().raiseMboxClearance (arg)
  })

  this.lowermbox = mkBase ((env,arg) => {
    assertIsNTuple(arg,2);
    assertIsCapability(arg.val[0]);
    assertIsAuthority(arg.val[1]);

    $t().lowerMboxClearance ( arg.val[0], arg.val[1])
  })

  
  this.monitorlocal = mkBase ((env, arg) => {
    assertNormalState("monitor");
    raiseCurrentThreadPC (arg.lev);
    assertIsProcessId (arg);

    let tid = arg.val; 

    // 1. find the thread corresponding to that tid 

    let t = __sched.__alive[tid.toString()];
    // 2. update the monitor state of that thread
    
    let r = rt_mkuuid ();
    if (t) {
      t.addMonitor (__sched.currentThreadId, r);
    }
    
    rt_ret(r);
  })


  this.demonitorlocal = mkBase ((env, arg) => {
    assertIsString(arg);
    // mutates state; so we should be careful...
    rt_ret (__unit);


  })

  this._setProcessDebuggingName = mkBase ((env,arg) => {
    assertIsString (arg)
    $t().processDebuggingName = arg.val
    rt_ret (__unit) 
  })

}




const rtObj = new RuntimeObject();

__theMailbox.setRuntimeObject(rtObj);

__sched.setRuntimeObject(rtObj);



function cleanup (cb) {
  readline.close();                    
  SS.stopCompiler(); 
  if (__p2pRunning) {
    (async () => {
      try {
      await p2p.stopp2p ()
    } catch (err) {
       debug ("stopping p2p ")
        if (err) {
            debug ("p2p stop failed " ,err)
        }
        else {
            debug ("p2p stop OK")                
        }
     
    }
    cb();
    }) ()    
  }
}


// 2020-02-09; AA; ugly ugly hack
function bulletProofSigint ()  {
  let listeners = process.listeners("SIGINT");
  // console.log (util.inspect(listeners))
  // for (let i = 0; i < listeners.length; i ++  ) {
    // console.log (listeners[i].toString());
    // }
    
  // process.stdin.removeAllListeners("on");
  process.removeAllListeners("SIGINT") ;
  // console.log ("sigint bulletproofing")
  process.on('SIGINT', () =>{  
    debug ("SIGINT")
    cleanup(( ) => {
      process.exit(0);
    }); 
  })  
  // setTimeout (bulletProofSigint, 1000)
}
bulletProofSigint();






async function start(f) {
  SS.setRuntimeObj(rtObj);
  // debug ("runing with uuid:", rt_uuid)

  function networkReady(p) {

    if (p) {
      debug ("network ready")
    } else {
      debug ("network not initialized")
    }

    let hostname = p;
    __nodeManager.setLocalHostPort(hostname);

    // first thing we do is link libraries
    // once that is done; the linker function
    // will call back to our starting function
    //
    f.loadlibs(() => {
      // debug ("callback ok");

      // obs; 2018-03-10;aa: the order of these
      // initializations is important.
      //

      let stopWhenAllThreadsAreDone = p == null ? true : false;
      __sched.initScheduler(__nodeManager.getLocalNode()
        , stopWhenAllThreadsAreDone
        , cleanup );

      let mainAuthority = new LVal(new Authority (levels.TOP), levels.BOT);

      __sched.scheduleNewThreadAtLevel (  
            f.main
          , [null,mainAuthority]
          , f
          , levels.BOT
          , levels.BOT
          , true
          , yargs.argv.persist
        )
      
      __sched.loop()

    })
  }

  const rtHandlers = {
    remoteSpawnOK : remoteSpawnOK,
    spawnFromRemote: spawnFromRemote,
    receiveFromRemote: receiveFromRemote,
    networkReady: networkReady,
    whereisFromRemote: whereisFromRemote
  };

  


  async function loadTrustMap (trustMapFile) {
    try {
      let s = await readFile (trustMapFile);
      let trustList = JSON.parse (s);
      let trustMap = {}
      trustList.map ( x => trustMap[x.id] = levels.mkLevel (x.level));
      _trustMap = trustMap;
    } catch (err) {
      logger.error ("cannot load trust map file: " + err);
    }  
  }


  if (yargs.argv.trustmap) {
    await loadTrustMap (yargs.argv.trustmap);
  } else {
    let default_trustmap = "trustmap.json"
    if (fs.existsSync (default_trustmap)) {
      await loadTrustMap (default_trustmap)
    }        
  }

  


  const allowRemoteSpawn = yargs.argv.rspawn
  if (allowRemoteSpawn) {
    if (allowRemoteSpawn=="true") {
      _allowRemoteSpawn = true;
    }
  }

  // 2020-02-01: rudimentary error handling of networking issues; AA
  // We mostly suppress connectivity issues, and propagate an error that 
  // we have not observed before.
  


  if (!yargs.argv.localonly && !yargs.argv.persist) {
    __p2pRunning = true;
  }

  const nodeIdFile = yargs.argv.id;
  if (nodeIdFile) {
    try{
      let nodeIdObj = await readFile(nodeIdFile)

      // process.on('unhandledRejection', (e) => p2p.processExpectedNetworkErrors(e, "unhandledRejection"))
      process.on ('unhandledRejection', up => {console.log ("Unhandled rejection"); console.error (up)})
      process.on ('uncaughtException', up => {console.log ("Uncaught exception"); console.error (up)})
      // process.on('uncaughtException', (e) => p2p.processExpectedNetworkErrors(e, "uncaughtException"))
      
      p2p.startp2p(JSON.parse(nodeIdObj), rtHandlers);

    } catch (err) {
      logger.error("cannot load id file")
      process.exit(1);
    }
  } else {
   try {
      if (yargs.argv.localonly || yargs.argv.persist) {
        info("Skipping network creation. Observe that all external IO operations will yield a runtime error.")
        if (yargs.argv.persist) {
          info("Running with persist flag.")
        }
        networkReady(null); // OBS: 2018-07-22: we are jumping over the network creation
      } else {        
        p2p.startp2p(null, rtHandlers);           
      }
    } catch (err) {
        logger.error ("uncaught exception in the runtime")
        console.error (err.stack);;
        process.exit(1);      
    }
  }
}


module.exports = {
  runtime: rtObj,
  start: start,
}
