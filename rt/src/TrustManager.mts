import * as levels from './Level.mjs';
import { getCliArgs, TroupeCliArg } from "./TroupeCliArgs.mjs";
import { Level } from "./Level.mjs";
import { __nodeManager } from "./NodeManager.mjs";
import { getConfigStore } from './ConfigStore.mjs';


const argv = getCliArgs();

let logLevel = argv[TroupeCliArg.Debug] ? 'debug' : 'info';
import { mkLogger } from './logger.mjs'
const logger = mkLogger('RTM', logLevel);


export let _trustMap = {}

export async function initTrustMap() {
    try {
        let trustList = await getConfigStore().trustList();
        if (trustList) {
            let trustMap = {}
            trustList.map(x => trustMap[x.id] = levels.mkLevel(x.level));
            _trustMap = trustMap;
        }
    } catch (err) {
        logger.error("cannot load trust map file: " + err);
    }
}


export function nodeTrustLevel(nodeid):Level {
    if (__nodeManager.isLocalNode(nodeid)) {
        return levels.ROOT
    }
    if (_trustMap) {
        // console.log ("true");
        return _trustMap[nodeid] ? _trustMap[nodeid] : levels.NULL;
    }
    return levels.NULL;
}