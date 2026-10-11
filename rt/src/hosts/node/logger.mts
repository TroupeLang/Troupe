// The Node host's loggers: winston, configured as the runtime had it before
// the hosts were separated. One console transport; the line is
// `timestamp [label] level: message`, the level coloured when colour is on.

import winston_pkg from 'winston';
import { Logger } from '../../logger.mjs';
import { isColorEnabled } from '../../colorConfig.mjs';

const { createLogger, format, transports } = winston_pkg;
const { combine, timestamp, label, printf } = format;

const lineFormat = printf(info =>
    `${info.timestamp} [${info.label}] ${info.level}: ${info.message}`);

export function winstonLogger(l: string, level: string): Logger {
    const formatList = [label({ label: `${l}` }), timestamp(), lineFormat];
    if (isColorEnabled()) {
        formatList.unshift(format.colorize());
    }
    return createLogger({
        level,
        format: combine(...formatList),
        transports: [new transports.Console()],
    });
}
