import levels  = require ('./options')
import { Level } from "./Level";
import * as Ty from './TroupeTypes';

export class LVal {
    val: any;
    lev: Level;
    tlev: Level;
    posInfo: string;


    __troupeType : Ty.TroupeType

    
    constructor(v:any, l:Level, tlev:Level = null, posInfo:string = null) {
        this.val = v;
        this.lev = l;

        this.tlev = tlev == null?l:tlev;
        this.posInfo = posInfo;
        this.__troupeType = Ty.getTroupeType(v);

    }

    get troupeType () {
        return this.__troupeType
    }

    get dataLevel () {
        return levels.TOP;// 2020-03-07; AA; placeholder
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
    constructor (x:LVal, l:Level, l2 = null) {
        if (l2 == null) {
            l2 = levels.lub (x.tlev,l)
        }
        super (x.val, levels.lub (x.lev, l), l2);
    }
}

export class LCopyVal extends LVal {
    constructor (x:LVal, l1:Level, l2:Level = null) {
        super (x.val, l1, l2);
    }
}



export class TLVal extends LVal {
   
}

export class MbVal extends LVal {

}


export function listStringRep(x, omitLevels = false, taintRef = null) {
  if (x.length == 0) {
    return "";
  }
  let s = x[0].stringRep(omitLevels, taintRef);

  for (let i = 1; i < x.length; i++) {
    s += ", " + x[i].stringRep(omitLevels, taintRef );
  }
  return s;
}