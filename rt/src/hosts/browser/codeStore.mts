// The browser's code store: libraries and the service module are fetched
// from where the page says they are; the sources an error report quotes
// are the texts the page supplied.
//
// A content-addressed module is served from the table the page handed the
// worker: the program's own modules by hash, as its compiled project carries
// them. On Node the dependencies file maps a hash to an artifact on disk
// (hosts/node/moduleResolver.mts); here the table is that mapping, and the
// file the program names is not consulted. A hash not in the table -- one a
// received closure references and this program does not have -- does not
// link, as on Node.

import { CodeStore, ArtifactNotLinkableError } from '../../CodeStore.mjs';

export interface BrowserCodeSources {
    /** Base URL of the compiled libraries, <base><Name>.js. */
    libraries: string;
    /** URL of the compiled trp-rt service module. */
    service: string;
    /** Program source texts by the path the compiler recorded. */
    sources: Record<string, string>;
    /** The program's modules by content hash: the code, and the name the
     *  dependencies file gives the module. */
    modules: Record<string, { code: string; name: string }>;
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
        seedModules(_depsFile: string): void {
            // The table is the seed; the file the program names is Node's.
        },

        async readArtifact(libname: string, _root: string | null): Promise<string> {
            if (libname.startsWith('module:')) {
                const m = code.modules[libname.slice('module:'.length)];
                if (m === undefined) throw new ArtifactNotLinkableError(this.displayName(libname));
                return m.code;
            }
            return await fetchText(code.libraries + libname + '.js');
        },

        loadService(): Promise<string> {
            return fetchText(code.service);
        },

        displayName(libname: string): string {
            if (!libname.startsWith('module:')) return libname;
            const m = code.modules[libname.slice('module:'.length)];
            return m ? `module:${m.name}` : libname;
        },

        readSource(filePath: string): string | null {
            return code.sources[filePath] ?? null;
        },
    };
}
