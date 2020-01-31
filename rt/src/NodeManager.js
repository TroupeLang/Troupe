'use strict'
class Node {
    constructor(nodeId) {
        this.nodeId = nodeId;     
    }
}

class NodeManager {   

    constructor (levels, aliases) {
        this.localNode = null;
        this.levels = levels;
        this.aliases = aliases
    }

    setLocalHostPort (h)  {
        if (this.localNode != null) {
            console.log ("error: local port already set. quitting...");
            process.exit(1);
        }
        this.localNode = new Node (h);
    }

    getNodeId () {
        this.localNode.nodeId
    }

    getNode(nodeName) {
        if (nodeName.startsWith ("@")) {
            nodeName = this.aliases[nodeName.substring(1)];
        }
        // TODO: error handling in case aliases are not available; 2020-01-31
        
        return new Node (nodeName);        
    }

    isLocalNode (id) {
        if (this.localNode == undefined) {
            console.log("ERROR: local node undefined; should not happen")
            process.exit(1);
        }
        return this.localNode.nodeId == id
    }

    // Another hack; 2018-03-10; aa
    getLocalNode() {
        if (this.localNode == undefined) {
            console.log("ERROR: local node undefined; should not happen")
            process.exit(1);
        }
        return this.localNode;
    }
}

module.exports = NodeManager;