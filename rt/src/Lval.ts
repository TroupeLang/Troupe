import levels  = require ('./options')
import { Level } from "./Level";
import * as Ty from './TroupeTypes';


export class LVal {
    val: any;
    lev: Level;
    tlev: Level;
    posInfo: string;
    highestAggregateLevel : Level;    
    
    constructor(v:any, l:Level, tlev:Level = null, posInfo:string = null) {
        this.val = v;
        this.lev = l;

        this.tlev = tlev == null?l:tlev;
        this.posInfo = posInfo;
        this.highestAggregateLevel = levels.TOP;  // conservative default
    }

    get highestLevel () {
        return levels.lub (this.highestAggregateLevel,this.tlev)
    }

    stringRep(omitLevels?: boolean, taintRef?: any) {
      let v = this.val; 
      let l = this.lev;
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


export class LValCopyAt extends LVal {
    constructor (x:LVal, l:Level) {
        super (x.val, levels.lub (x.lev, l), levels.lub (x.tlev,l));
        this.highestAggregateLevel = x.highestAggregateLevel;
    }
}

export class LCopyVal extends LVal {
    constructor (x:LVal, l1:Level, l2:Level = null) {
        super (x.val, l1, l2);
        this.highestAggregateLevel = x.highestAggregateLevel;
    } 
}



export class TLVal extends LVal {
    troupeType : Ty.TroupeType
    constructor(v:any, l: Level, tlev:Level = null, posInfo:string = null) {
        super (v,l,tlev, posInfo )
        this.troupeType = Ty.getTroupeType(v);
        if (Ty.isBaseType(this.troupeType)) {
            this.highestAggregateLevel = levels.BOT
        } else if (this.troupeType == Ty.TroupeType.LVAL) {
            this.highestAggregateLevel = v.highestAggregateLevel
        }
    }
}

export class MbVal extends LVal {

}