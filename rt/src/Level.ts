import { TroupeType } from "./TroupeTypes";
import { TroupeRawValue } from "./TroupeRawValue";
// import levels from './options';

export abstract class Level implements TroupeRawValue {
  lev: any;
  isLevel: boolean = true ;
  _troupeType: TroupeType = TroupeType.LEVEL
  abstract dataLevel;

  constructor(lev) {
    this.lev = lev;        
  }    

  stringRep () {
    return this.lev.toString();
  }

}
