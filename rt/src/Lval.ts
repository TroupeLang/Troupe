import levels  = require ('./options')
import { Level } from "./Level";
import * as Ty from './TroupeTypes';


export class LVal {
    val: any;
    lev: Level;
    tlev: Level;
    posInfo: string;
    stringRep: (omitLevels?: boolean, taintRef?: any) => string;
    highestLevel : Level;    
    constructor(v:any, l:Level, tlev:Level = null, posInfo:string = null) {
        this.val = v;
        this.lev = l;

        this.tlev = tlev == null?l:tlev;
        this.posInfo = posInfo;
        this.highestLevel = levels.TOP;

        this.stringRep = (omitLevels = false, taintRef = null) => {
            let t = "";
            if (v.stringRep != undefined) { // 2018-05-17; AA; ugly hack!
                t = v.stringRep(omitLevels, taintRef)
            } else {
                if (typeof v === 'string') {
                    t = "\"" + v.toString() + "\""
                } else {
                    t = v.toString();
                }
            }

            if (l.stringRep == undefined) {
                console.log("undefined strringrep", l);
            }

            let s = t;

            if (!omitLevels) {
                s = s + "@" + l.stringRep() + "%" + this.tlev.stringRep();
            }
            
            if (taintRef) {
                taintRef.lev = levels.lub (taintRef.lev, l);
            }
            
            return s;
        }
    }
}

export class TLVal extends LVal {
    troupeType : Ty.TroupeType
    constructor(v:any, l: Level, tlev:Level = null, posInfo:string = null) {
        super (v,l,tlev, posInfo )
        this.troupeType = Ty.getTroupeType(v);
        if (Ty.isBaseType(this.troupeType)) {
            this.highestLevel = this.tlev;
        }
    }
}