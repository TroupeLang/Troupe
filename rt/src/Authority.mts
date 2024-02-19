import { Level } from "./Level.mjs";
import { BOT } from "./options.mjs";

import { TroupeRawValue } from "./TroupeRawValue.mjs";
import { TroupeType } from "./TroupeTypes.mjs";

export class Authority implements TroupeRawValue {
    authorityLevel: Level;
    stringRep: () => string;
    _troupeType = TroupeType.AUTHORITY 
    dataLevel = BOT;
    constructor (authorityLevel: Level) {
        this.authorityLevel = authorityLevel
        this.stringRep = this.toString;
    }

    toString () {
        let x = this.authorityLevel.stringRep();
        return "!" + x;
    } 
}

