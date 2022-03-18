// 2018-07-21: AA; A very basic logging setup ... Not particularly attached to
// this library or this way of doing things, but this still beats console
// outputs. 

const { createLogger, format, transports } = require('winston');
const { combine, timestamp, label, printf } = format;

const myFormat = printf(info => {
  //  return `${info.timestamp} ${info.level}: ${info.message}`;
  return `${info.timestamp} [${info.label}] ${info.level}: ${info.message}`;
});

export function mkLogger (l, level='info') {  
    
  const consol = new transports.Console();  

  let x =  createLogger({
              level : level, // comment out this file to remove debug messages
              format: combine(
                format.colorize(),        
                label({ label: `${l}` }),
                timestamp(),
                myFormat
              ),
              transports: [consol]
           });
  return x
}



