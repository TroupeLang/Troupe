// The node's connection to other nodes.
//
// The runtime core reaches the network through this interface; the libp2p
// stack under p2p/ is the node host's implementation of it. A host that
// registers none runs as --localonly does: every remote operation fails.

/**
 * A network error to report to the user without a stack trace, the way
 * CliValidationError reports a command-line error.
 */
export class P2pUserError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'P2pUserError';
    }
}

export interface NetworkProvider {
    /** Starts the network with the runtime's handlers; resolves to the peer id. */
    startp2p(nodeId: any, rtHandlers: any): Promise<String>;
    spawnp2p(id: string, data: any): Promise<any>;
    sendp2p(id: string, procId: any, obj: any): any;
    whereisp2p(id: string, data: any): Promise<any>;
    stopp2p(): Promise<any>;
    processExpectedNetworkErrors(err: any, source?: string): any;
}

let __network: NetworkProvider | null = null;

export function registerNetworkProvider(provider: NetworkProvider): void {
    __network = provider;
}

export function getNetworkProvider(): NetworkProvider {
    if (__network == null) {
        throw new Error('no network on this node: the host registered no network provider');
    }
    return __network;
}
