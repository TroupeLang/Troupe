// The browser's configuration store: the parsed structures the page handed
// the worker in the run request.

import { ConfigStore } from '../../ConfigStore.mjs';

export interface BrowserConfig {
    aliases?: Record<string, string>;
    trustList?: any[];
    nodeIdentity?: any;
}

export function browserConfigStore(config: BrowserConfig): ConfigStore {
    return {
        aliases: () => config.aliases ?? {},
        trustList: async () => config.trustList ?? null,
        nodeIdentity: async () => config.nodeIdentity ?? null,
    };
}
