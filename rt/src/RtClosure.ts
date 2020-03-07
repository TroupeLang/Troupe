import {TroupeAggregateRawValue} from './TroupeRawValue'
import {TroupeType} from './TroupeTypes'
import levels = require('./levels/tagsets');

export class RtClosure implements TroupeAggregateRawValue{
  env: any;
  fun: any 
  namespace: any 
  _troupeType: TroupeType
  
  stringRep (omitLevels = false) {
    return "fn => .."
  }

  get dataLevel () {
    return levels.TOP  // 2020-03-08; AA; conservative placeholder; 
                       // this needs to change and be computed
                       // through the iteration over the environment
                       // 
  }

  constructor(e:any, p:any, f:any) {
    this.env = e;
    this.fun = f;
    this.namespace = p
    this._troupeType = TroupeType.CLOSURE
  }
}
