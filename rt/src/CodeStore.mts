// Where compiled Troupe artifacts come from.
//
// The loader consumes artifact *source text* and instantiates it with
// `new Function('rt', text)` (loadLibsAsync.mts). Nothing above this interface
// knows about files, paths or URLs -- only artifact identity. That is the whole
// of what a host has to supply for code loading: Node reads from the install
// tree, another host can fetch or carry the text however it likes.
//
// Library caching, dependency recursion, the datatype skew check and every
// diagnostic stay in the loader; only the retrieval moves.

/**
 * A module identity that this node cannot link: the program records no
 * dependencies, or none of them has this hash. A received module-bearing
 * closure naming a module the receiver does not have is rejected with this,
 * all-or-nothing -- it never links.
 *
 * It is a distinct type, rather than a distinguished return value, so that a
 * store which simply fails to retrieve an artifact cannot have that reported
 * as "not among this program's dependencies".
 */
export class ArtifactNotLinkableError extends Error {
    constructor(displayName: string) {
        super(`cannot link module ${displayName}: it is not `
            + `among this program's dependencies`);
        this.name = 'ArtifactNotLinkableError';
    }
}

export interface CodeStore {
    /**
     * Seed content-addressed module identities from the main program's
     * dependencies file. A module's identity is its hash, which cannot
     * reconstruct a location on its own.
     */
    seedModules(depsFile: string): void;

    /**
     * Source text for an artifact: a library by bare name ("List") or a
     * content-addressed module ("module:<hash>").
     *
     * `root` is the caller's own module root when it has one; a received
     * closure has none and passes null.
     *
     * Throws ArtifactNotLinkableError when a module identity is not locally
     * discoverable, and whatever the retrieval itself raises otherwise.
     */
    readArtifact(libname: string, root: string | null): Promise<string>;

    /** Source text of the trp-rt service module, loaded on every start. */
    loadService(): Promise<string>;

    /** Renders "module:<hash>" as "module:<name>" for diagnostics. */
    displayName(libname: string): string;

    /**
     * The text of a program source file, for the context printed with an
     * error, or null when it is not available. Synchronous: it runs while
     * an error is being reported.
     */
    readSource(filePath: string): string | null;
}

let __codeStore: CodeStore | null = null;

export function registerCodeStore(store: CodeStore): void {
    __codeStore = store;
}

export function getCodeStore(): CodeStore {
    if (__codeStore == null) {
        throw new Error(
            'no code store registered: the host must call registerCodeStore() '
            + 'before the runtime loads any library or the service module');
    }
    return __codeStore;
}
