import {TroupeType} from './TroupeTypes'

export class BaseFunction {
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
  }

  
