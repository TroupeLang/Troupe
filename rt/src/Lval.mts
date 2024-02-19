import * as levels from './options.mjs'
import { Level } from "./Level.mjs";
import * as Ty from './TroupeTypes.mjs';
import { TroupeRawValue } from './TroupeRawValue.mjs';

export class LVal implements TroupeRawValue{
    val: any;
    lev: Level;
    tlev: Level;
    dlev: Level;
    posInfo: string;

    /* 2020-06-06: AA   

       Observe that we only need the type information here only because of the 
       base type such as booleans, strings, and numbers; becauase we cannot attach
       properties to them in JS. 

       The main downside of duplicating the type information is the duplication of 
       this information during serialization
    */
    __troupeType : Ty.TroupeType 
    
    constructor(v:any, l:Level, tlev:Level = null, posInfo:string = null) {
        this.val = v;
        this.lev = l;
        this.tlev = tlev == null?l:tlev;
        this.posInfo = posInfo;
        if (v._troupeType == undefined) {
            this.__troupeType = Ty.getTypeForBasicValue(v)
            this.dlev = this.lev
        } else {
            this.__troupeType = v._troupeType                 
            this.dlev = levels.lub (this.lev, v.dataLevel)
        } 
    }

    get _troupeType() {
        return Ty.TroupeType.LVAL
    }
    get troupeType () {        
        return this.__troupeType
    }

    get dataLevel () {
        return this.dlev;
    }

    get closureType ()  {
        return (this.troupeType == Ty.TroupeType.CLOSURE 
                    ? this.val._closureType 
                    : null
               )
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