// Where a node's configuration comes from: its identity, its alias map and
// its trust map. The store returns parsed structures; where they are kept --
// files named on a command line, a page's settings -- is the host's.

export interface ConfigStore {
    /** The alias map, {} when none is configured. Read once, at startup. */
    aliases(): Record<string, string>;

    /** The trust list, one `{id, level}` per entry, or null when none is
     *  configured. Rejects when the configured one cannot be read. */
    trustList(): Promise<any[] | null>;

    /** The node's identity, or null when none is configured. Rejects, with a
     *  message to report, when the configured one cannot be read. */
    nodeIdentity(): Promise<any | null>;
}

let __configStore: ConfigStore | null = null;

export function registerConfigStore(store: ConfigStore): void {
    __configStore = store;
}

export function getConfigStore(): ConfigStore {
    if (__configStore == null) {
        throw new Error(
            'no configuration store registered: the host must call '
            + 'registerConfigStore() before the runtime is imported');
    }
    return __configStore;
}
