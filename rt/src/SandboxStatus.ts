class NormalState  {
    name:string
    constructor (name="NORMAL") {
      this.name = name
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
}

class InHandlerState extends NormalState {
    trapper: any;
    constructor (f) {
        super ("INHANDLER");
        this.trapper = f;
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
    constructor (f) {
        super ("INSANDBOX");
        this.trapper = f;
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

