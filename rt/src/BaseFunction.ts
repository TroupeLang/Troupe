export class BaseFunction {
  env: any;
  fun: any;
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
    }
  }

  
