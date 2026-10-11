// The node host: every registration the runtime needs in place before it is
// imported. The compiler service and the network provider are registered by
// the entry point once the runtime is loaded, since they need it.

import { registerCliArgsParser } from '../../TroupeCliArgs.mjs';
import { parseCliArgsFromProcess } from './cliArgs.mjs';
import { registerLoggerFactory } from '../../logger.mjs';
import { winstonLogger } from './logger.mjs';
import { registerCodeStore } from '../../CodeStore.mjs';
import { nodeCodeStore } from './codeStore.mjs';
import { registerConfigStore } from '../../ConfigStore.mjs';
import { nodeConfigStore } from './configStore.mjs';
import { registerPersistStore } from '../../PersistStore.mjs';
import { nodePersistStore } from './persistStore.mjs';
import { registerResultChannel } from '../../ResultChannel.mjs';
import { nodeResultSocket } from './resultSocket.mjs';
import { registerTerminal } from '../../Terminal.mjs';
import { nodeTerminal } from './terminal.mjs';
import { registerCompression } from '../../Compression.mjs';
import { nodeCompression } from './compression.mjs';
import { registerHostConsole } from '../../hostConsole.mjs';
import { nodeConsole } from './console.mjs';

export function registerNodeHost(): void {
    registerCliArgsParser(parseCliArgsFromProcess);
    registerLoggerFactory(winstonLogger);
    registerCodeStore(nodeCodeStore);
    registerConfigStore(nodeConfigStore);
    registerPersistStore(nodePersistStore);
    registerResultChannel(nodeResultSocket);
    registerTerminal(nodeTerminal);
    registerCompression(nodeCompression);
    registerHostConsole(nodeConsole());
}
