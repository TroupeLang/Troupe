'use strict'
import * as fs from 'node:fs'
import * as levels from './Level.mjs';
const { readFile } = fs.promises

import { getTroupeRoot } from './troupeRoot.mjs'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('lib')

const info = x => logger.info(x)
const debug = x => logger.debug(x)

// A loaded library: its exported value functions keyed by declaration name,
// and the exported datatype group hashes it embeds (this.__datatypeHashes, set
// by the compiler for library builds).
type LoadedLib = { values: Record<string, any>, datatypeHashes: string[] }

// Memoized per-process library loader. Each library is read from disk,
// instantiated, its own dependencies linked, and its export table computed
// exactly once; the in-flight promise is cached so concurrent requests share
// one instantiation. Both value linking and the datatype skew check go through
// here, so a library is never loaded or instantiated more than once.
const __libcache: Record<string, Promise<LoadedLib>> = {}

function loadLib(libname: string, rtObj): Promise<LoadedLib> {
    if (__libcache[libname]) {
        debug('lib cache hit on: ' + libname)
        return __libcache[libname]
    }
    const p = (async (): Promise<LoadedLib> => {
        // Libraries are loaded from a default location.
        let filename = getTroupeRoot() + "/lib/out/" + libname + ".js"
        let input = await readFile(filename, 'utf8')
        // TODO: check for error! 2018-07-03: aa
        let Lib: any = new Function('rt', input)
        let libinstance = new Lib(rtObj)

        // Link this library's own dependencies before computing its table.
        await loadLibsAsync(libinstance, rtObj)

        // export() is a compiler-generated function returning the value table;
        // libload mode is the runtime's hack for evaluating it (2019-01-03: AA).
        rtObj.setLibloadMode()
        let table = libinstance.export({__dataLevel:levels.BOT}).val.toArray()
        rtObj.setNormalMode()

        let values: Record<string, any> = Object.create(null)
        for (let i = 0; i < table.length; i++) {
            let name = table[i].val[0].val
            if (!(name in values)) values[name] = table[i].val[1].val
        }
        return { values, datatypeHashes: libinstance.__datatypeHashes || [] }
    })()
    __libcache[libname] = p
    return p
}

export async function loadLibsAsync(obj, rtObj) {
    let libs = obj.libs
    obj.libs = {}
    for (let n = 0; n < libs.length; n++) {
        let lib = libs[n].lib
        let decl = libs[n].decl
        let loaded = await loadLib(lib, rtObj)
        if (decl in loaded.values) {
            obj.libs[lib + "." + decl] = loaded.values[decl]
        }
    }

    // Datatype version-skew check (normalization.md §10). Enforced here, in the
    // single universal library-load path, but kept as a self-contained pass so
    // it is easy to locate and reason about independently of value linking.
    await checkConsumedDatatypes(obj, rtObj)
}

// Verify that every datatype group hash the artifact consumed at compile time
// is still among the loaded library's exported group hashes (membership, not
// equality: library extension is harmless, a changed consumed group is not).
async function checkConsumedDatatypes(obj, rtObj) {
    let consumed = obj.__consumedDatatypeHashes
    if (!consumed) return
    for (let libname of Object.keys(consumed)) {
        let exported = (await loadLib(libname, rtObj)).datatypeHashes
        for (let h of consumed[libname]) {
            if (!exported.includes(h)) {
                throw new Error(
                    "datatype version skew: library '" + libname + "' no longer exports a "
                    + "datatype group that was consumed at compile time (group hash " + h
                    + "). The importer was compiled against a different version of '" + libname
                    + "'; recompile it against the current library.")
            }
        }
    }
}
