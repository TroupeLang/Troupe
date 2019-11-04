class NormalState  {
    constructor () {
        
    }
    isNormal () {
        return true;
    }

    declassificationAllowed() {
        return true;
    }
}

class InHandlerState extends NormalState {
    trapper: any;
    constructor (f) {
        super ();
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
        super ();
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

