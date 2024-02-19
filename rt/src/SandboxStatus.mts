import { Level } from "./Level.mjs";

export interface HnState {
    isNormal ():  boolean
    checkGuard () : void 
    getTrapper (): any 
    declassificationAllowed () : boolean
    lev : Level
}
class NormalState implements HnState   {
    name:string
    constructor (name="NORMAL") {
      this.name = name
    }


    checkGuard () {

    }

    isNormal () {
        return true;
    }

    declassificationAllowed() {
        return true;
    }

    toString () {
      return this.name
    }

    getTrapper () {
        return null;
    }

    get lev () {
        throw new Error ("handler usage error")
        return null;
    }
}

class InHandlerState extends NormalState {
    trapper: any;
    _lev : Level
    constructor (f,lev, checkGuard ) {
        super ("INHANDLER");
        this.trapper = f;
        this._lev = lev 
        this.checkGuard = checkGuard 
    }

    get lev () {
        return this._lev
    }

    isNormal () {
        return false;
    }

    getTrapper() {
        return this.trapper;
    }

    declassificationAllowed () {
        return false;
    }
     
}


class InSandboxState extends NormalState {
    trapper: any;
    _lev : Level 
    constructor (f, lev ) {
        super ("INSANDBOX");
        this.trapper = f;
        this._lev = lev 
    }

    get lev () {
        return this._lev 
    }

    isNormal () {
        return false;
    }

    getTrapper() {
        return this.trapper;
    }
}

export const HandlerState = { 
    NORMAL : NormalState,
    INHANDLER : InHandlerState,
    INSANDBOX : InSandboxState
}

export default HandlerState
