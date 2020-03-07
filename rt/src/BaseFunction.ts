import {TroupeType} from './TroupeTypes'
import {TroupeAggregateRawValue} from './TroupeRawValue'
import levels = require('./levels/tagsets');

export class BaseFunction implements TroupeAggregateRawValue{
  env: any;
  fun: any;
  _troupeType: TroupeType;

  stringRep: () => string;

  constructor(f, name=null) {
    this.env = null;
    this.fun = f;
    this.stringRep = () => {
      if (name) {
        return `<basefun:${name}>`
      } else {
        return "<basefun:_>"
      }
    }
    this._troupeType = TroupeType.CLOSURE;
  }

  get dataLevel () {
    return levels.BOT; // there is nothing confidential in the data 
                       // contained in the base functions
  }

}

  
