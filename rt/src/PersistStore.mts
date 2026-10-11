// Where persisted values are kept. The builtins name a value's place as a
// path; the store maps the path to its medium.

export interface PersistStore {
    /** Writes the serialized form of a value at the path, replacing it. */
    write(path: string, text: string): void;

    /** The serialized form stored at the path. Rejects when there is none. */
    read(path: string): Promise<string>;
}

let __persistStore: PersistStore | null = null;

export function registerPersistStore(store: PersistStore): void {
    __persistStore = store;
}

export function getPersistStore(): PersistStore {
    if (__persistStore == null) {
        throw new Error('no persistence store registered on this host');
    }
    return __persistStore;
}
