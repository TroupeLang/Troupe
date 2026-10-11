// Importing this module registers the node host. For a harness whose
// imports of the runtime are static: listed first, it runs before them.

import { registerNodeHost } from './index.mjs';

registerNodeHost();
