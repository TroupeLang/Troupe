// The browser's persistence store: the origin private file system, under a
// directory the page names. A path is taken relative to that directory;
// "./out/saved.x.persist.json" is the file saved.x.persist.json in its
// subdirectory out, created as needed.
//
// write() returns before the write lands -- the interface is synchronous
// because Node's store is -- so writes are queued in order and a read waits
// for the queue, which keeps a save followed by a restore consistent.

import { PersistStore } from '../../PersistStore.mjs';

type DirectoryHandle = {
    getDirectoryHandle(name: string, opts?: { create?: boolean }): Promise<DirectoryHandle>;
    getFileHandle(name: string, opts?: { create?: boolean }): Promise<FileHandle>;
};
type FileHandle = {
    createWritable(): Promise<{ write(data: string): Promise<void>; close(): Promise<void> }>;
    getFile(): Promise<{ text(): Promise<string> }>;
};

export function hasOriginPrivateFileSystem(): boolean {
    const nav = (globalThis as any).navigator;
    return typeof nav?.storage?.getDirectory === 'function';
}

function segments(path: string): string[] {
    const parts = path.split('/').filter((p) => p !== '' && p !== '.');
    if (parts.length === 0 || parts.some((p) => p === '..')) {
        throw new Error(`persistence: cannot store at ${path}`);
    }
    return parts;
}

export function opfsPersistStore(rootName: string): PersistStore {
    let root: Promise<DirectoryHandle> | null = null;
    let queue: Promise<void> = Promise.resolve();

    function rootDir(): Promise<DirectoryHandle> {
        root ??= (globalThis as any).navigator.storage.getDirectory()
            .then((d: DirectoryHandle) => d.getDirectoryHandle(rootName, { create: true }));
        return root;
    }

    async function fileHandle(path: string, create: boolean): Promise<FileHandle> {
        const parts = segments(path);
        let dir = await rootDir();
        for (const p of parts.slice(0, -1)) {
            dir = await dir.getDirectoryHandle(p, { create });
        }
        return await dir.getFileHandle(parts[parts.length - 1], { create });
    }

    return {
        write(path: string, text: string): void {
            segments(path);  // a path that cannot be stored is reported now, not later
            queue = queue.then(async () => {
                const w = await (await fileHandle(path, true)).createWritable();
                await w.write(text);
                await w.close();
            });
            queue.catch((e) => console.error(`persistence: cannot write ${path}: ${e.message}`));
        },

        async read(path: string): Promise<string> {
            await queue;
            const file = await (await fileHandle(path, false)).getFile();
            return await file.text();
        },
    };
}
