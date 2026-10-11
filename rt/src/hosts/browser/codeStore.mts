// The browser's code store: libraries and the service module are fetched
// from where the page says they are; the sources an error report quotes
// are the texts the page supplied.
//
// Content-addressed modules are not served: their identity is a hash, and
// the dependencies file that maps a hash to an artifact is a path on the
// compiling machine (hosts/node/moduleResolver.mts). A program that uses
// modules, or a received closure that references one, does not link here.

import { CodeStore, ArtifactNotLinkableError } from '../../CodeStore.mjs';

export interface BrowserCodeSources {
    /** Base URL of the compiled libraries, <base><Name>.js. */
    libraries: string;
    /** URL of the compiled trp-rt service module. */
    service: string;
    /** Program source texts by the path the compiler recorded. */
    sources: Record<string, string>;
}

async function fetchText(url: string): Promise<string> {
    const r = await fetch(url);
    if (!r.ok) {
        throw new Error(`cannot fetch ${url}: ${r.status} ${r.statusText}`);
    }
    return await r.text();
}

export function browserCodeStore(code: BrowserCodeSources): CodeStore {
    return {
        seedModules(depsFile: string): void {
            throw new Error(
                `the program records a dependencies file (${depsFile}); `
                + 'content-addressed modules are not available on this host');
        },

        async readArtifact(libname: string, _root: string | null): Promise<string> {
            if (libname.startsWith('module:')) {
                throw new ArtifactNotLinkableError(libname);
            }
            return await fetchText(code.libraries + libname + '.js');
        },

        loadService(): Promise<string> {
            return fetchText(code.service);
        },

        displayName(libname: string): string {
            return libname;
        },

        readSource(filePath: string): string | null {
            return code.sources[filePath] ?? null;
        },
    };
}
