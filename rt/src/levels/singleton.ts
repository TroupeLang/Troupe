
import { Level }  from '../Level.js'


class Singleton extends Level {
    isTop = true
    get dataLevel () {
        return __theLevel; // observe delayed 
    }
    constructor (lev) {
        super(lev);
    }

    stringRep () {
        return "{-}"
        
    }
}

let __theLevel = new Singleton(0)


function lub (...ls:Level[]):Level {
    return __theLevel
}


function glb (l1:Level, l2:Level):Level {
    return __theLevel
}

function flowsTo (l1:Level, l2:Level):boolean {
    return true;
    
}



function fromString (str2): Level {
    return __theLevel    
}



function lubs (x) {
    return __theLevel
  
}


let levels = {
    BOT: __theLevel,
    TOP: __theLevel,
    lub: lub,
    glb: glb,
    flowsTo: flowsTo,
    mkLevel :fromString,
    lubs    
}


export = levels;
