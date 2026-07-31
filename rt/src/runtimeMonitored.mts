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
import * as levels from './Level.mjs'
import * as DS from './deserialize.mjs'
import { p2p, P2pUserError } from './p2p/p2p.mjs'
import { closeReadline } from './builtins/stdio.mjs';
import { ttyRestore } from './builtins/tty.mjs';
import { __theRegister } from './builtins/whereis.mjs';
import { assertIsFunction } from './Asserts.mjs'
import runId from './runId.mjs'
import { __nodeManager } from './NodeManager.mjs'
import { setRuntimeObject } from './SysState.mjs';
import { initTrustMap, nodeTrustLevel, _trustMap } from './TrustManager.mjs';
import { serialize } from './serialize.mjs';
import { Thread } from './Thread.mjs';
import { ErrorKind } from './TroupeError.mjs';

import { Console } from 'node:console'

const { flowsTo, actsFor, lub, glb } = levels
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { connectResultSocket, sendSocketMessageAndClose } from './resultSocket.mjs';
import { configureColors, isColorEnabled } from './colorConfig.mjs';
import { mkLogger, mkDebugTag } from './logger.mjs'
import { getTroupeRoot } from './troupeRoot.mjs'
import { Record } from './Record.mjs';
import { level } from 'winston';
import { shouldDrop, extractQuarantineAuth } from './QuarantineUtils.mjs';
import { DCLabel } from './levels/DCLabels/dclabel.mjs';
import { implies } from './levels/DCLabels/cnf.mjs';

const readFile = fs.promises.readFile
const rt_uuid = runId
const argv = getCliArgs();

// Configure colors before any chalk or logger usage
configureColors();

let logLevel = argv[TroupeCliArg.Debug] ? 'debug': 'info'
const logger = mkLogger('RTM', logLevel);

const info = x => logger.info(x)
const debug = mkDebugTag(logger)
const error = x => logger.error(x)

// Quarantine-specific logger
const qrnLogLevel = argv[TroupeCliArg.DebugQuarantine] ? 'debug' : 'info';
const qrnLogger = mkLogger('QRN', qrnLogLevel);
const qdebug = mkDebugTag(qrnLogger);

let __p2pRunning = false;

// Flag to prevent compiler exit handler from interfering with intended exit code
export let __exitInitiated = false;
export function setExitInitiated() {
    __exitInitiated = true;
}


let rt_xconsole = 
      new Console({ stdout: process.stdout
                  , stderr: process.stderr
                  , colorMode: isColorEnabled()
                 });

function $t():Thread { return __sched.__currentThread }; // returns the current thread

// --------------------------------------------------

async function spawnAtNode(nodeid, f) {
  debug (`* rt spawnAtNode  ${nodeid}`);
  let node = __nodeManager.getNode(nodeid.val);
  // debug ("XX", node);

  // TODO: 2018-09-24: AA: do the information flow check

  let { data, level } = serialize(f, lub($t().pc, nodeid.lev), node.nodeId);

  let trustLevel = nodeTrustLevel(node.nodeId);
  let theThread = $t();

  if (!implies(trustLevel.confidentiality, level.confidentiality)) { // }, { node: node.nodeId })) {
    theThread.throwInSuspended("Illegal trust flow when spawning on a remote node\n" +
      ` | the trust level of the recepient node: ${trustLevel.stringRep()}\n` +
      ` | the level of the information in spawn: ${level.stringRep()}`)
     __sched.scheduleThread(theThread);
     __sched.resumeLoopAsync();  
     return;
  }


  // 0. we assume that the node is different from
  //    the local node

  // 1. we make a connection to the remote node
  // 2. we send the serialized version of f
  // 3. we wait for the reply (should be a pid)
  // 4. we return the obtained pid
  //--------------------------------------------------



  try {
    let body1 = await p2p.spawnp2p(node.nodeId, data);
    let result = await DS.deserialize(nodeTrustLevel(node.nodeId), body1, node.nodeId)

    // For spawn responses, DROP means we can't trust the pid we got back
    if (shouldDrop(result)) {
      error(`Dropping corrupt spawn response from ${node.nodeId}`);
      theThread.throwInSuspended("Corrupt spawn response from remote node");
      __sched.scheduleThread(theThread);
      __sched.resumeLoopAsync();
      return;
    }

    let body = result.value!;
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

let _allowRemoteSpawn = argv[TroupeCliArg.RSpawn];
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

  let result;
  try {
    result = await DS.deserialize(nodeLev, jsonObj, fromNode)
  } catch (e) {
    // Same expected-adversarial-input disposition as receiveFromRemote: a
    // spawned closure that names a module or library this node does not have
    // makes deserialize reject. Drop the spawn (return null, handled by the
    // caller) rather than let the rejection reach the fire-and-forget p2p
    // dispatch and terminate the node. Unexpected failures still surface.
    if (DS.isExpectedInboundError(e)) {
      debug(`Rejecting spawn from ${fromNode}: ${(e as Error).message}`);
      return null;
    }
    throw e;
  }

  // For spawn requests, DROP means we reject the spawn
  if (shouldDrop(result)) {
    debug(`Rejecting spawn from ${fromNode} due to corrupt data`);
    // Return null to indicate failure - the caller will handle this
    return null;
  }

  let lf = result.value!;
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

  let serObj = serialize(newPid, levels.BOT, fromNode).data
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

  let result;
  try {
    result = await DS.deserialize(nodeTrustLevel(fromNode), jsonObj, fromNode);
  } catch (e) {
    // A received value we cannot reconstruct is an expected adversarial input
    // (e.g. a closure that names a module or library the receiver does not
    // have): drop it and keep the node running, the same disposition as the
    // corrupt-data drop below. Reaching here without a catch would leave the
    // promise returned to the fire-and-forget p2p SEND handler rejected, and
    // the process-level unhandledRejection handler terminates the node.
    if (DS.isExpectedInboundError(e)) {
      debug(`Dropping unreconstructable message from ${fromNode}: ${(e as Error).message}`);
      qdebug(`DROP: message from ${fromNode} could not be deserialized: ${(e as Error).message}`);
      return;  // Silent drop
    }
    throw e;  // Unexpected failure: do not mask it
  }

  // Handle ingress check result
  if (shouldDrop(result)) {
    debug(`Dropping corrupt message from ${fromNode}`);
    qdebug(`DROP: message from ${fromNode} contained corrupt data`);
    return;  // Silent drop
  }

  const data = result.value!;
  debug `* rt receiveFromremote *  ${fromNode} ${data}`;

  let toPid = new LVal(new ProcessID(rt_uuid, pid, __nodeManager.getLocalNode()), data.lev);

  // Pass quarantine authority if present
  const quarantineAuth = extractQuarantineAuth(result);

  if (quarantineAuth !== null) {
    qdebug `QUARANTINE: message from ${fromNode} quarantined with auth ${quarantineAuth}`;
  }

  // Pass raw fromNode; addMessage will construct the labeled value using
  // the receiving thread's creation-time PC
  __theMailbox.addMessage(fromNode, toPid, data.val, data.lev, quarantineAuth);
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
/**
 * Send message to remote node.
 *
 * @param toPid The pid of the remote process
 * @param message The data to send
 * @param qauth Optional quarantine authority for sending quarantined data
 */
function sendMessageToRemote(toPid, message, qauth?: Authority) {
  let node = toPid.node.nodeId;
  let pid = toPid.pid;

  let { data, level } = serialize(new MbVal(message, $t().pc), $t().pc, node);

  let trustLevel = nodeTrustLevel(node);

  // Key change: only coalesce if qauth provided
  // REMOVED: { node } option - no more automatic wildcard coalescing
  let effectiveTrust = qauth
    ? trustLevel.coalesce(qauth.authorityLevel)
    : trustLevel;

  // if (!actsFor(effectiveTrust, level)) {  // No { node } option!
  if (!implies(effectiveTrust.confidentiality, level.confidentiality)) {
    threadError("Illegal trust flow when sending information to a remote node\n" +
      ` | the trust level of the recepient node: ${trustLevel.stringRep()}\n` +
      (qauth ? ` | effective trust (with qauth): ${effectiveTrust.stringRep()}\n` : '') +
      ` | the level of the information to send:  ${level.stringRep()}`, false, null, ErrorKind.IFCCheck);
  } else {
    p2p.sendp2p(node, pid, data)
    return $t().returnImmediateLValue(__unit);
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

function rt_sendMessageNochecks(lRecipientPid, message, qauth?: Authority, ret = true) {
  let recipientPid = lRecipientPid.val;

  if (isLocalPid(recipientPid)) {
    __theMailbox.addMessage(__nodeManager.getNodeId(), lRecipientPid, message, $t().pc);

    if (ret) {
      return $t().returnImmediateLValue(__unit);
    }
  } else {
    debug ("* rt rt_send remote *");
    return sendMessageToRemote(recipientPid, message, qauth);
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
    // chalk.red(formatToN("_sp:" + $t()._sp, 20)),
    s 
  );
}



async function whereisFromRemote(k, fromNode) {
  __sched.resumeLoopAsync()
  // TODO: 2018-10-20: make use of the levels as they were
  // recorded during the registration (instead of the bottom here )
  if (__theRegister[k]) {
    let serObj = serialize(__theRegister[k], levels.BOT, fromNode).data
    return serObj
  }
}



function rt_mkLabel(x) {
  // debug ("mkLabel", x, x === "secret");

  
  return new LVal(levels.fromSingleTag(x), $t().pc);

}




function threadError(s, internal = false, explainer = null, errorKind: ErrorKind = ErrorKind.DynTypeError) {
  return $t().threadError(s, internal, explainer, errorKind);
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
  await sendSocketMessageAndClose({ type: 'process-exit', exitCode: 0 });
  ttyRestore()
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
      await sendSocketMessageAndClose({ type: 'process-exit', exitCode: 0 });
      await cleanupAsync()
      process.exit(0);
    })()
  })
  // setTimeout (bulletProofSigint, 1000)
}
bulletProofSigint();



async function loadServiceCode() {
  let input = await fs.promises.readFile(getTroupeRoot() + '/trp-rt/out/service.js', 'utf8')
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
  const nodeIdFile = argv[TroupeCliArg.Id] as string;

  // Parse node ID from file if provided
  let nodeId = null;
  if (nodeIdFile) {
    try {
      const nodeIdObj = await readFile(nodeIdFile, 'utf-8')
      nodeId = JSON.parse(nodeIdObj);
    } catch (err) {
      logger.error(`cannot load id file: ${nodeIdFile}`)
      process.exit(1);
    }
  }

  // Handle local-only or persist modes (skip network creation)
  if (argv[TroupeCliArg.LocalOnly] || argv[TroupeCliArg.Persist]) {
    if (!argv[TroupeCliArg.SuppressLocalInfoMessage]) {
      info("Skipping network creation. Observe that all network operations will yield a runtime error.")
    }
    if (argv[TroupeCliArg.Persist]) {
      info("Running with persist flag.")
    }
    return null // OBS: 2018-07-22: we are jumping over the network creation
  }

  // Start P2P network
  try {
    process.on('unhandledRejection', (e) => p2p.processExpectedNetworkErrors(e, "unhandledRejection"))
    process.on('uncaughtException', (e) => p2p.processExpectedNetworkErrors(e, "uncaughtException"))
    return await p2p.startp2p(nodeId, rtHandlers);
  } catch (err) {
    if (err instanceof P2pUserError) {
      logger.error(err.message);
    } else {
      logger.error("p2p network initialization failed")
      console.error(err.stack);
    }
    process.exit(1);
  }
}

export async function start(f) {
  await connectResultSocket();
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
  let mainAuthority = new LVal(new Authority(levels.ROOT), levels.BOT);

  if (__p2pRunning) {
    let service_arg = 
      new LVal ( new Record([ ["authority", mainAuthority], 
                              ["options", __unit]]), 
              levels.BOT);
    __sched.scheduleNewThreadAtLevel(__service['service']
          , service_arg
          , levels.TOP
          , levels.BOT
          , false
          , null
          , true);
  }

  __sched.scheduleNewThreadAtLevel(
    () => f.main ({__dataLevel:levels.BOT})
    , mainAuthority
    // , f
    , levels.BOT
    , levels.BOT
    , true
    , argv[TroupeCliArg.Persist]
  )
  __sched.loop()

  // Set up execution timeout if specified via --timeout
  const timeoutSeconds = argv[TroupeCliArg.Timeout] as number;
  if (timeoutSeconds > 0) {
    const exitCode = (argv[TroupeCliArg.TimeoutExitCode] as number) ?? 124;
    const timer = setTimeout(async () => {
      console.error(`Execution timed out after ${timeoutSeconds} seconds`);
      setExitInitiated();
      await sendSocketMessageAndClose({ type: 'process-exit', exitCode, reason: 'timeout' });
      await cleanupAsync();
      process.exit(exitCode);
    }, timeoutSeconds * 1000);
    timer.unref(); // Don't prevent natural exit when all threads complete
  }
}

