const logger = require('../logger.js').mkLogger('TAGSETS');
const info = x => logger.info(x)
const debug = x => logger.debug(x)

import { create } from 'peer-id';
import { string } from 'yargs';
import { Level }  from '../Level.js'


function stringRep (T) {
    let n = T.size
    let s = "{";
    let i = 0;
    let R = Array.from (T.values()).sort();

    R.forEach( t => {
        s += t;
        if (++ i < n ) {
            s += ","
        }
    })
    s += "}"
    return s;
}


let __cache = {}


class TagLevel extends Level {
    isTop: boolean;
    __stringRep : string ;
   
    constructor (lev, s = null) {
        s = s || stringRep (lev);
        super(lev);
        this.__stringRep = s
      
    }

    stringRep () {
        return this.__stringRep;
    }

    get dataLevel () {
        return botLevel;
    }


}

// Factory method for creating tag levels 
// Observe that it makes use of caching 
function createTagLevel0 ( lev ) {
    let s = stringRep (lev)
    __cache [s] = __cache[s] || (new TagLevel (lev))
    return __cache[s]
}

let botLevel = createTagLevel0 (new Set ())
// botLevel.dataLevel = botLevel ; // 2021-03-19; hacky; AA


function createTagLevel ( lev ) {
    let obj = createTagLevel0 (lev);    
    // obj.dataLevel = botLevel ;
    return obj;
}



let topLevel = new TagLevel ({}, "{#TOP}");
topLevel.isTop = true;




function lub (...ls:Level[]):Level {
    if (ls.length == 2) {
        if (ls[0] == ls[1]) {
            return ls[0]
        }

    }  

    let s = new Set ();
    for (let l of ls) {
        if (l == topLevel) {
            return topLevel
        }
        l.lev.forEach(t => s.add(t));
    }
    return createTagLevel (s); 
}


function glb (l1:Level, l2:Level):Level {
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
    return createTagLevel (s);
}

function flowsTo (l1:Level, l2:Level):boolean {
    if (l1 == l2) {
        return true;
    }
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



function fromString (str2): Level {
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
    return createTagLevel (s);
}



function lubs (x) {
  if (x.length == 0) {
    return levels.BOT;
  } else {
    let r = x[0];
    for (let i = 1; i < x.length; i++) {
      r = lub (r, x[i]);
    }
    return r;
  }
}


let levels = {
    BOT: botLevel, 
    TOP: topLevel,
    lub: lub,
    glb: glb,
    flowsTo: flowsTo,
    mkLevel :fromString,
    lubs    
}


export = levels;
