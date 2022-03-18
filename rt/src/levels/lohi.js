/* 2020-05-19; this method is commented for convenience to avoid 
            having the IDE erroneously suggest importing it 
*/

/*
const Level = require('../Level.js');

let levels = {
    LOW: new Level(0),
    HIGH: new Level(1)
};

levels.BOT = levels.LOW;
levels.TOP = levels.HIGH;

levels.LOW.stringRep = () => "{public}"

levels.HIGH.stringRep = () => "`{secret}`"


levels.lub = (l1, l2) => {
    if (l1.lev > l2.lev)
        return l1;
    return l2;
}

levels.glb = (l1, l2) => {
    if (l1.lev > l2.lev)
        return l2;
    return l1;
}

levels.flowsTo = (l1, l2) => {
    return (l1.lev <= l2.lev);
}

levels.mkLevel = (x) => {
    if ( x === "secret" || x === "`{secret}`" ) {
        return levels.HIGH;
    } else {
        if ( x === "public" || x === "{public}" ) { 
            return levels.LOW
        }
        else {
            throw ("Level unknown:" + x)
        }
    }
}

module.exports = levels;
*/