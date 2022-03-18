import levels from "./options";
import yargs from "yargs";
import fs from 'fs'
import { Level } from "./Level";
import { __nodeManager } from "./NodeManager";
const { readFile } = fs.promises



let logLevel = yargs.argv.debug ? 'debug' : 'info';
const logger = require('./logger.js').mkLogger('RTM', logLevel);


export let _trustMap = {}

async function loadTrustMap(trustMapFile) {
    try {
        let s = await readFile(trustMapFile, 'utf-8');
        let trustList = JSON.parse(s);
        let trustMap = {}
        trustList.map(x => trustMap[x.id] = levels.mkLevel(x.level));
        _trustMap = trustMap;
    } catch (err) {
        logger.error("cannot load trust map file: " + err);
    }
}


export async function initTrustMap() {
    if (yargs.argv.trustmap) {
        await loadTrustMap(yargs.argv.trustmap);
    } else {
        let default_trustmap = "trustmap.json"
        if (fs.existsSync(default_trustmap)) {
            await loadTrustMap(default_trustmap)
        }
    }

}


export function nodeTrustLevel(nodeid):Level {
    if (__nodeManager.isLocalNode(nodeid)) {
        return levels.TOP
    }
    if (_trustMap) {
        // console.log ("true");
        return _trustMap[nodeid] ? _trustMap[nodeid] : levels.BOT;
    }
    return levels.BOT;
}