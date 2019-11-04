const logger = require('../logger.js').mkLogger('TAGSETS');
const info = x => logger.info(x)
const debug = x => logger.debug(x)

import { Level }  from '../Level.js'

class TagLevel extends Level {
    isTop: boolean;
    constructor (lev) {
        super(lev);
    }

    stringRep () {
        let n = this.lev.size
        let s = "{";
        let i = 0;
        this.lev.forEach( t => {
            s += t;
            if (++ i < n ) {
                s += ","
            }
        })
        s += "}"
        return s;
    }
}

let topLevel = new TagLevel ({});
topLevel.stringRep = () => "{#TOP}"
topLevel.isTop = true;

function lub (l1, l2):any {
    // return topLevel;
    if (l1 == topLevel || l2 == topLevel) {
        return topLevel;
    }
    // debug (l1.lev.toString());
    // debug (l2);

    let s = new Set ();
    l1.lev.forEach(t => s.add(t));
    l2.lev.forEach(t => s.add(t));
    return new TagLevel (s);    
}

function glb (l1, l2):any {
    if (l1 == topLevel) {
        return l2;
    }

    if (l2 == topLevel ) {
        return l1;
    }

    let s = new Set();
    l1.lev.forEach (
        t => {
            if (l2.lev.has(t)) {
              s.add(t);
            }
        });
    return new TagLevel (s);
}

function flowsTo (l1, l2) {
    if (l2 == topLevel) {
        return true;
    }

    if (l1 == topLevel) {
      return (l2 == topLevel);
    }

    const iter = l1.lev.entries();
    for (let t1 of iter) {
        if (!l2.lev.has(t1[0])) {
          return false;
        }
    }

    return true;
}



function fromString (str2) {
    // debug (str2.toString())
    // the implementation is slightly over-protected
    // to deal with {} issues; 2018-09-19; AA

    const str1 = str2.trim();
    const str = str1.startsWith ("{") && str1.endsWith ("}") ?
               str1.substring(1, str1.length - 1) :
               str1;

    if (str == "#TOP") {
        return topLevel;
    }

    let s = new Set ();
    const tags = str.split(',');

    tags.map ( t => {
        if ( t != '') {
          s.add (t.trim().toLowerCase());
        }
    });
    return new TagLevel (s);
}


let levels = {
    BOT: new TagLevel (new Set ()),
    TOP: topLevel,
    lub: lub,
    glb: glb,
    flowsTo: flowsTo,
    mkLevel :fromString
}


export = levels;
