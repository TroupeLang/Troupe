
export class Level {
  lev: any;
  isLevel: boolean;
    constructor(lev) {
      this.lev = lev;
      this.isLevel = true;
    }    

    stringRep () {
      return this.lev.toString();
    }

  }
