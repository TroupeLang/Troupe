import {__unitbase} from './UnitBase.mjs'
import * as levels from './options.mjs';
import { LVal } from './Lval.mjs';

export let __unit = new LVal (__unitbase, levels.BOT, levels.BOT)

