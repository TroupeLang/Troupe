// 2018-07-21: AA; A very basic logging setup ... Not particularly attached to
// this library or this way of doing things, but this still beats console
// outputs. 

import winston_pkg from 'winston';
const { createLogger, format, transports } = winston_pkg
const { combine, timestamp, label, printf } = format;
import { isColorEnabled } from './colorConfig.mjs';

const myFormat = printf(info => {
  //  return `${info.timestamp} ${info.level}: ${info.message}`;
  return `${info.timestamp} [${info.label}] ${info.level}: ${info.message}`;
});

export function mkLogger (l, level='info') {  
    
  const consol = new transports.Console();  

  // Conditionally include colorize based on color configuration
  const formatList = [
    label({ label: `${l}` }),
    timestamp(),
    myFormat
  ];
  
  if (isColorEnabled()) {
    formatList.unshift(format.colorize());
  }

  let x =  createLogger({
              level : level, // comment out this file to remove debug messages
              format: combine(...formatList),
              transports: [consol]
           });
  return x
}

/** A lazy debug logger for hot paths, used as a tagged template literal:
 *
 *      const debug = mkDebugTag(logger);
 *      debug `delivering ${message} at ${pc}`
 *
 *  When debug logging is off the tag returns before the message string is
 *  built, so interpolated values are never stringified — unlike an ordinary
 *  template literal argument, which is fully constructed before the logger
 *  can discard it. Values with a stringRep() method (runtime values,
 *  levels, pids) are rendered with it; everything else with String().
 *
 *  The enabled check is captured at creation time: log levels in this
 *  runtime are fixed at startup from CLI flags.
 */
export function mkDebugTag (logger) {
  const enabled = logger.isLevelEnabled('debug');
  return (strings: TemplateStringsArray | any, ...vals: any[]) => {
    if (!enabled) return;
    if (!Array.isArray(strings)) {
      // called as a plain function, debug(msg) — kept for migration
      logger.debug(strings);
      return;
    }
    let s = strings[0];
    for (let i = 0; i < vals.length; i++) {
      const v = vals[i];
      s += (v != null && typeof v.stringRep === 'function' ? v.stringRep() : String(v))
           + strings[i + 1];
    }
    logger.debug(s);
  };
}



