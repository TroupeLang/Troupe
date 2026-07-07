'use strict'

import * as fs from 'node:fs'
import * as levels from "./Level.mjs";
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';
import { ImplementationError } from './TroupeError.mjs';
import { mkLogger } from './logger.mjs';
const argv = getCliArgs();
const logger = mkLogger('NodeManager');


class Node {
    nodeId: any;
    constructor(nodeId) {
        this.nodeId = nodeId;     
    }
}

class NodeManager {
    localNode: any;
    levels: any
    aliases: any;
    
    constructor () {

        let aliases = argv[TroupeCliArg.Aliases]
                        ? JSON.parse ( fs.readFileSync(argv[TroupeCliArg.Aliases] as string, 'utf8'))
                        : {}

        
        this.localNode = null;
        this.levels = levels;
        this.aliases = aliases
    }

    setLocalPeerId (peerid)  {
        if (this.localNode != null) {
            logger.error ("local node identity already set");
            throw new ImplementationError ("local node identity already set");
        }
        this.localNode = new Node (peerid);
    }

    getNodeId () {
        if (this.localNode.nodeId == null) {
            return "<local>"
        } 
        return this.localNode.nodeId
    }

    getNode(nodeName) {
        if (nodeName.startsWith ("@")) {
            nodeName = this.aliases[nodeName.substring(1)];
        }
        // TODO: error handling in case aliases are not available; 2020-01-31
        
        return new Node (nodeName);        
    }

    isLocalNode (id) {        
        if (id == "<null>") {
            return true;
        }
        if (this.localNode == undefined) {
            logger.error("local node undefined; should not happen")
            throw new ImplementationError("local node undefined")
        }
        return this.localNode.nodeId == this.getNode(id).nodeId
    }

    // Another hack; 2018-03-10; aa
    getLocalNode() {
        if (this.localNode == undefined) {
            logger.error("local node undefined; should not happen")
            throw new ImplementationError("local node undefined")
        }
        return this.localNode;
    }
}

export let __nodeManager = new NodeManager()