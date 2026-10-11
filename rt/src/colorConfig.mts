import chalk from 'chalk';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';

let colorConfigured = false;

export function configureColors(): void {
    if (colorConfigured) return; // Ensure we only configure once
    
    const argv = getCliArgs();
    
    if (argv[TroupeCliArg.NoColor]) {
        chalk.level = 0; // Disable all colors
    }
    
    colorConfigured = true;
}

export function isColorEnabled(): boolean {
    const argv = getCliArgs();
    return !argv[TroupeCliArg.NoColor];
}