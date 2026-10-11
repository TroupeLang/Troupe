// Node's persistence store: a path is a file.

import * as fs from 'node:fs';
import { PersistStore } from '../../PersistStore.mjs';

export const nodePersistStore: PersistStore = {
    write(path: string, text: string): void {
        fs.writeFileSync(path, text);
    },

    read(path: string): Promise<string> {
        return fs.promises.readFile(path, 'utf8');
    },
};
