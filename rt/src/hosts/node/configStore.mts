// Node's configuration store: the files named on the command line.

import * as fs from 'node:fs';
import { getCliArgs, TroupeCliArg } from '../../TroupeCliArgs.mjs';
import { ConfigStore } from '../../ConfigStore.mjs';

const { readFile } = fs.promises;

export const nodeConfigStore: ConfigStore = {
    aliases(): Record<string, string> {
        const path = getCliArgs()[TroupeCliArg.Aliases] as string | undefined;
        return path ? JSON.parse(fs.readFileSync(path, 'utf8')) : {};
    },

    // Precedence: --trustmap, then trustmap.json in the working directory.
    async trustList(): Promise<any[] | null> {
        const argv = getCliArgs();
        let path = argv[TroupeCliArg.Trustmap] as string | undefined;
        if (!path) {
            const default_trustmap = "trustmap.json";
            if (!fs.existsSync(default_trustmap)) return null;
            path = default_trustmap;
        }
        return JSON.parse(await readFile(path, 'utf-8'));
    },

    async nodeIdentity(): Promise<any | null> {
        const path = getCliArgs()[TroupeCliArg.Id] as string | undefined;
        if (!path) return null;
        try {
            return JSON.parse(await readFile(path, 'utf-8'));
        } catch (err) {
            throw new Error(`cannot load id file: ${path}`);
        }
    },
};
