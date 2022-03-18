const level = require('./options.js');


theBaseUnit = { stringRep: () => "()"
              , _is_unit : true
              , _troupeType : 0 /* UNIT */
              , dataLevel : level.BOT 
              } ;

module.exports = theBaseUnit;