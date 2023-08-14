import * as fs from 'node:fs';
import chalk from 'chalk';
import { v4 as uuidv4 } from 'uuid'
import AggregateError from 'aggregate-error';
import { __unit } from './UnitVal.mjs'
import { Authority } from './Authority.mjs'
import { Scheduler } from './Scheduler.mjs'
import { MailboxProcessor } from './MailboxProcessor.mjs'
import { RuntimeInterface } from './RuntimeInterface.mjs'
import { LVal, MbVal } from './Lval.mjs'
import { ProcessID } from './process.mjs';
import { UserRuntime } from './UserRuntime.mjs'
import * as levels from './options.mjs'
import * as DS from './deserialize.mjs'
import { p2p } from './p2p/p2p.mjs'
import { closeReadline } from './builtins/stdio.mjs';
import { __theRegister } from './builtins/whereis.mjs';
import { assertIsFunction } from './Asserts.mjs'
import runId from './runId.mjs'
import { __nodeManager } from './NodeManager.mjs'
import { setRuntimeObject } from './SysState.mjs';
import { initTrustMap, nodeTrustLevel, _trustMap } from './TrustManager.mjs';
import { serialize } from './serialize.mjs';
import { Thread } from './Thread.mjs';

import { Console } from 'node:console'

const { flowsTo, lub, glb } = levels
import yargs from 'yargs'
const readFile = fs.promises.readFile
const rt_uuid = runId


let logLevel = yargs.argv.debug ? 'debug': 'info'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('RTM', logLevel);

const info = x => logger.info(x)
const debug = x => logger.debug(x)
const error = x => logger.error(x)

let __p2pRunning = false;


let rt_xconsole = 
      new Console({ stdout: process.stdout
                  , stderr: process.stderr
                  , colorMode:true
                 });

function $t():Thread { return __sched.__currentThread }; // returns the current thread

// --------------------------------------------------

async function spawnAtNode(nodeid, f) {
  debug (`* rt spawnAtNode  ${nodeid}`);
  let node = __nodeManager.getNode(nodeid.val);
  // debug ("XX", node);

  // TODO: 2018-09-24: AA: do the information flow check

  let { data, level } = serialize(f, lub($t().pc, nodeid.lev));

  let trustLevel = nodeTrustLevel(node.nodeId);

  if (!flowsTo(level, trustLevel)) {
    threadError("Illegal trust flow when spawning on a remote node\n" +
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
    let body = await DS.deserialize(nodeTrustLevel(node.nodeId), body1)
    let pid = new ProcessID(body.val.uuid, body.val.pid, body.val.node);
    theThread.returnSuspended(new LVal(pid, body.lev));

    __sched.scheduleThread(theThread);
    __sched.resumeLoopAsync();

  } catch (err) {
    error("error spawning remotely; this blocks current thread")
    if (err instanceof AggregateError) {
      for (let ie in err) {
        error(`${ie}`)
      }
    } else {
      error(`${err}`)
    }
  }
}

let _allowRemoteSpawn = (yargs.argv.rspawn) && (yargs.argv.rspawn == "true")
function remoteSpawnOK() {
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
  debug ("spawn from remote")
  // 2018-05-17: AA; note that this _only_ uses the lf.lev and
  // is completely independent of the current thread's pc;

  let nodeLev = nodeTrustLevel(fromNode);

  let lf = await DS.deserialize(nodeLev, jsonObj)
  let f = lf.val;
  let newPid =
    __sched.scheduleNewThreadAtLevel(
      f
      , __unit //[f.env, __unit]
      // , f.namespace
      , lf.lev
      , lf.lev
    );

  // 2018-09-19: AA: because we need to send some info back, we have to invoke
  // serialization.

  let serObj = serialize(newPid, levels.BOT).data
  __sched.resumeLoopAsync();
  return (serObj);
}


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
  debug(`* rt receiveFromremote *  ${JSON.stringify(jsonObj)}`)
  let data = await DS.deserialize(nodeTrustLevel(fromNode), jsonObj)
  debug(`* rt receiveFromremote *  ${fromNode} ${data.stringRep()}`);

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
  // debug (`* rt *  ${toPid}  ${message.stringRep()}`);  

  let { data, level } = serialize(new MbVal(message, $t().pc), $t().pc);

  // debug (`* rt *  ${JSON.stringify(data)}`);
  let trustLevel = nodeTrustLevel(node);

  // debug ("data level: " +  level.stringRep());
  // debug ("remote trust level: " + trustLevel.stringRep());

  if (!flowsTo(level, trustLevel)) {
    threadError("Illegal trust flow when sending information to a remote node\n" +
      ` | the trust level of the recepient node: ${trustLevel.stringRep()}\n` +
      ` | the level of the information to send:  ${level.stringRep()}`);
  } else {
    p2p.sendp2p(node, pid, data)
    return $t().returnImmediateLValue(__unit);   // we return unit to the call site at the thread level
  }
}

// TODO: AA; 2020-05-19; consider moving these two functions somewhere else

function isLocalPid(pid) {
  let x = pid.uuid.toString() == rt_uuid.toString();
  return (x);
}

function rt_mkuuid() {
  let pid = uuidv4();
  let uuidval = $t().mkVal(pid);
  return uuidval;
}

function rt_sendMessageNochecks(lRecipientPid, message, ret = true) {
  let recipientPid = lRecipientPid.val;
  // debug (`rt_sendMessageNoChecks ${message.stringRep()}`)

  if (isLocalPid(recipientPid)) {
    let nodeId = __sched.mkVal(__nodeManager.getNodeId());
    __theMailbox.addMessage(nodeId, lRecipientPid, message, $t().pc);

    if (ret) {
      return $t().returnImmediateLValue(__unit);      
    }
  } else {
    debug ("* rt rt_send remote *"/*, recipientPid, message*/);
    return sendMessageToRemote(recipientPid, message)
  }
}




let rt_debug = function (s) {
  function formatToN(s, n) {
    if (s.length < n) {
      let j = s.length;
      for (; j < n; j++) {
        s = s + " ";
      }
    }
    return s;
  }

  let tid = $t().tidErrorStringRep()
  let pc = $t().pc.stringRep()
  let bl = $t().bl.stringRep()
  let handler_state = __sched.handlerState.toString()
  rt_xconsole.log(
    chalk.red(formatToN("PID:" + tid, 50)),
    chalk.red(formatToN("PC:" + pc, 20)),
    chalk.red(formatToN("BL:" + bl, 20)),
    chalk.red(formatToN("HN" + handler_state, 20)),
    chalk.red(formatToN("_sp:" + $t()._sp, 20)),
    s 
  );
}



async function whereisFromRemote(k) {
  __sched.resumeLoopAsync()
  // TODO: 2018-10-20: make use of the levels as they were
  // recorded during the registration (instead of the bottom here )
  if (__theRegister[k]) {
    let serObj = serialize(__theRegister[k], levels.BOT).data
    return serObj
  }
}



function rt_mkLabel(x) {
  // debug ("mkLabel", x, x === "secret");

  return new LVal(levels.mkLevel(x), $t().pc);

}




function threadError(s, internal = false) {
  return $t().threadError(s, internal);
}

let rt_threadError = threadError;

function rt_error(x) {
  threadError(x.val);
}

function rt_errorPos(x, pos) {
  if (pos != '') {
    threadError(x.val + " at " + pos);
  } else {
    threadError(x.val);
  }
}


let rt_ret = (arg) => { return $t().returnImmediateLValue(arg); } 
// let rt_ret_raw = () => __sched.returnInThread_raw();

// function tailcall(lff, arg) {
//   assertIsFunction(lff);
//   $t().raiseCurrentThreadPC(lff.lev);
//   __sched.tailToTroupeFun(lff.val, arg);
// }

let __sched: Scheduler
let __theMailbox: MailboxProcessor
let __userRuntime: any
let __service:any = {}

class RuntimeObject implements RuntimeInterface {
  // tailcall = tailcall
  xconsole = rt_xconsole
  ret = rt_ret
  // ret_raw = rt_ret_raw 
  debug = rt_debug
  spawnAtNode = spawnAtNode
  rt_mkuuid = rt_mkuuid
  mkLabel = rt_mkLabel
  sendMessageNoChecks = rt_sendMessageNochecks;
  cleanup = cleanupAsync
  persist(obj, path) {
    let jsonObj = serialize(obj, $t().pc).data;
    fs.writeFileSync(path, JSON.stringify(jsonObj));
  }

  get $service () {
    return __service
  }
  
  get $t() {
    return $t()
  }

  get __sched() {
    return __sched
  }

  get __mbox() {
    return __theMailbox
  }

  get __userRuntime() {
    return __userRuntime
  }

  constructor() {
    __sched = new Scheduler(this)
    __theMailbox = new MailboxProcessor(this)
    __userRuntime = new UserRuntime(this)
  }

}


let __rtObj = new RuntimeObject();
DS.setRuntimeObj(__rtObj.__userRuntime);
setRuntimeObject(__rtObj)



async function cleanupAsync() {
  closeReadline()
  DS.stopCompiler();
  if (__p2pRunning) {
    try {
      debug("stopping p2p")
      await p2p.stopp2p()
      debug("p2p stop OK")
    } catch (err) {
      debug(`p2p stop failed ${err}`)
    }
  }
}


// 2020-02-09; AA; ugly ugly hack
function bulletProofSigint() {
  let listeners = process.listeners("SIGINT");
  // console.log (util.inspect(listeners))
  // for (let i = 0; i < listeners.length; i ++  ) {
  // console.log (listeners[i].toString());
  // }

  // process.stdin.removeAllListeners("on");
  process.removeAllListeners("SIGINT");
  // console.log ("sigint bulletproofing")
  process.on('SIGINT', () => {
    debug("SIGINT");
    (async () => {
      await cleanupAsync()
      process.exit(0);
    })()
  })
  // setTimeout (bulletProofSigint, 1000)
}
bulletProofSigint();



async function loadServiceCode() {
  let input = await fs.promises.readFile(process.env.TROUPE + '/trp-rt/out/service.js', 'utf8')
  let S: any = new Function('rt', input)
  let service = new S(__userRuntime);

  await __userRuntime.linkLibs(service)

  __userRuntime.setLibloadMode()
  let table = service.export({__dataLevel:levels.BOT}).val.toArray()
  __userRuntime.setNormalMode()

  for (let i = 0; i < table.length; i++) {
    let name = table[i].val[0].val
    let ff = table[i].val[1].val
    __service[name] = ff
  }
}



async function getNetworkPeerId(rtHandlers) {
  const nodeIdFile = yargs.argv.id as string;
  if (nodeIdFile) {
    try {
      let nodeIdObj = await readFile(nodeIdFile, 'utf-8')
      process.on('unhandledRejection', (e) => p2p.processExpectedNetworkErrors(e, "unhandledRejection"))
      // process.on ('unhandledRejection', up => {console.log ("Unhandled rejection"); console.error (up)})
      // process.on ('uncaughtException', up => {console.log ("Uncaught exception"); console.error (up)})
      process.on('uncaughtException', (e) => p2p.processExpectedNetworkErrors(e, "uncaughtException"))
      return await p2p.startp2p(JSON.parse(nodeIdObj), rtHandlers);
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
        return null//  OBS: 2018-07-22: we are jumping over the network creation
      } else {
        return await p2p.startp2p(null, rtHandlers);
      }
    } catch (err) {
      logger.error("uncaught exception in the runtime")
      console.error(err.stack);;
      process.exit(1);
    }
  }
}

export async function start(f) {
  await initTrustMap()

  let peerid = await getNetworkPeerId({
    remoteSpawnOK,
    spawnFromRemote,
    receiveFromRemote,
    whereisFromRemote
  })

  if (peerid) {
    __p2pRunning = true
    debug("network ready")
  } else {
    debug("network not initialized")
  }

  __nodeManager.setLocalPeerId(peerid);

  let stopWhenAllThreadsAreDone = !__p2pRunning
  __sched.initScheduler(__nodeManager.getLocalNode()
    , stopWhenAllThreadsAreDone
    , cleanupAsync);

  await loadServiceCode()
  await __userRuntime.linkLibs(f)
  let mainAuthority = new LVal(new Authority(levels.TOP), levels.BOT);

  __sched.scheduleNewThreadAtLevel(
    () => f.main ({__dataLevel:levels.BOT})
    , mainAuthority
    // , f
    , levels.BOT
    , levels.BOT
    , true
    , yargs.argv.persist
  )
  __sched.loop()
}

