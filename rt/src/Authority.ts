import { Level } from "./Level";
import { BOT } from "./options";

import { TroupeRawValue } from "./TroupeRawValue";
import { TroupeType } from "./TroupeTypes";

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

