import { Level } from "./Level";

export class Authority {
    authorityLevel: Level;
    stringRep: () => string;
    constructor (authorityLevel: Level) {
        this.authorityLevel = authorityLevel
        this.stringRep = this.toString;
    }

    toString () {
        let x = this.authorityLevel.stringRep();
        return "!" + x;
    } 
}

