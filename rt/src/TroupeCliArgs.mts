import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';

/**
 * Error class for CLI validation failures.
 * These errors are displayed to users without stack traces.
 */
class CliValidationError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'CliValidationError';
    }
}

/**
 * Handles CLI errors gracefully.
 * For CliValidationError, prints a clean message and exits.
 * For other errors, rethrows to preserve the original stack trace.
 */
function handleCliError(error: unknown): never {
    if (error instanceof CliValidationError) {
        console.error(`Error: ${error.message}`);
        process.exit(1);
    }
    throw error;
}

export enum TroupeCliArg {
    Debug = 'debug',
    DebugSandbox = 'debugsandbox',
    DebugMailbox = 'debugmailbox',
    DebugP2p = 'debugp2p',
    DebugQuarantine = 'debugquarantine',
    Pini = 'pini',
    Nmifc = 'nmifc',
    ShowStack = 'showStack',
    Trustmap = 'trustmap',
    Id = 'id',
    LocalOnly = 'localonly',
    Persist = 'persist',
    IoRoot = 'io-root',
    Aliases = 'aliases',
    Stdiolev = 'stdiolev',
    StdioModel = 'stdio-model',
    Port = 'port',
    File = 'file',
    RSpawn = 'rspawn',
    Relay = 'relay',
    RelayOnly = 'relay-only',
    NoP2pCircuit = 'no-p2p-circuit',
    DisableRelay = 'disable-relay',
    RelayFaultTolerance = 'relay-fault-tolerance',
    NoColor = 'no-color',
    LabelFormat = 'label-format',
    SuppressLocalInfoMessage = 'suppress-local-info-message',
    SuppressMainThreadFinishedMessage = 'suppress-main-thread-finished-message',
    Explain = 'explain',
    Timeout = 'timeout',
    TimeoutExitCode = 'timeout-exit-code',
    ResultSocket = 'result-socket',
}

export interface ParsedArgs {
    [TroupeCliArg.Debug]?: boolean;
    [TroupeCliArg.DebugSandbox]?: boolean;
    [TroupeCliArg.DebugMailbox]?: boolean;
    [TroupeCliArg.DebugP2p]?: boolean;
    [TroupeCliArg.DebugQuarantine]?: boolean;
    [TroupeCliArg.Pini]?: boolean;
    [TroupeCliArg.Nmifc]?: boolean;
    [TroupeCliArg.ShowStack]?: boolean;
    [TroupeCliArg.Trustmap]?: string;
    [TroupeCliArg.Id]?: string;
    [TroupeCliArg.LocalOnly]?: boolean;
    [TroupeCliArg.Persist]?: boolean;
    [TroupeCliArg.IoRoot]?: string;
    [TroupeCliArg.Aliases]?: string;
    [TroupeCliArg.Stdiolev]?: string;
    [TroupeCliArg.StdioModel]?: string;
    [TroupeCliArg.Port]?: number;
    [TroupeCliArg.File]?: string;
    [TroupeCliArg.RSpawn]?: boolean;
    [TroupeCliArg.Relay]?: string | string[];
    [TroupeCliArg.RelayOnly]?: boolean;
    [TroupeCliArg.NoP2pCircuit]?: boolean;
    [TroupeCliArg.DisableRelay]?: boolean;
    [TroupeCliArg.RelayFaultTolerance]?: string;
    [TroupeCliArg.NoColor]?: boolean;
    [TroupeCliArg.LabelFormat]?: string;
    [TroupeCliArg.SuppressLocalInfoMessage]?: boolean;
    [TroupeCliArg.SuppressMainThreadFinishedMessage]?: boolean;
    [TroupeCliArg.Explain]?: boolean;
    [TroupeCliArg.Timeout]?: number;
    [TroupeCliArg.TimeoutExitCode]?: number;
    [TroupeCliArg.ResultSocket]?: string;
    [key: string]: any;
}

let parsedArgs: ParsedArgs | null = null;

export function getCliArgs(): ParsedArgs {
    if (!parsedArgs) {
        const rawArgs = yargs(hideBin(process.argv))
            .option(TroupeCliArg.Debug, { alias: 'd', type: 'boolean', default: false, describe: 'Enable general debug logging' })
            .option(TroupeCliArg.DebugSandbox, { type: 'boolean', default: false, describe: 'Enable debug logging for sandbox operations' })
            .option(TroupeCliArg.DebugMailbox, { type: 'boolean', default: false, describe: 'Enable debug logging for mailbox processing' })
            .option(TroupeCliArg.DebugP2p, { type: 'boolean', default: false, describe: 'Enable debug logging for P2P communication' })
            .option(TroupeCliArg.DebugQuarantine, { type: 'boolean', default: false, describe: 'Enable debug logging for quarantine operations' })
            .option(TroupeCliArg.Pini, { type: 'boolean', default: false, describe: 'Enable Pini mode for declassification' })
            .option(TroupeCliArg.Nmifc, { type: 'boolean', default: true, describe: 'Enable NMIFC (Non-Malleable IFC) enforcement for downgrades (use --no-nmifc to disable)' })
            .option(TroupeCliArg.ShowStack, { alias: 'ss', type: 'boolean', default: false, describe: 'Show stack traces on errors' })
            .option(TroupeCliArg.Trustmap, { alias: 'tm', type: 'string', describe: 'Path to the trustmap JSON file' })
            .option(TroupeCliArg.Id, { alias: 'i', type: 'string', describe: 'Path to the node ID file' })
            .option(TroupeCliArg.LocalOnly, { alias: 'l', type: 'boolean', default: false, describe: 'Run in local-only mode, skipping network creation' })
            .option(TroupeCliArg.Persist, { alias: 'P', type: 'boolean', default: false, describe: 'Enable persistence mode' })
            .option(TroupeCliArg.IoRoot, { type: 'string', describe: 'Directory subtree SimpleFileIO may access; paths resolve relative to it and cannot escape it. Defaults to a per-invocation scratch dir when unset.' })
            .option(TroupeCliArg.Aliases, { alias: 'a', type: 'string', describe: 'Path to the aliases JSON file' })
            .option(TroupeCliArg.Stdiolev, { type: 'string', describe: 'Security level for stdio operations' })
            .option(TroupeCliArg.StdioModel, {
                type: 'string',
                choices: ['capability', 'ifc'],
                default: 'capability',
                describe: 'Stdio enforcement model: capability (acquisition is checked against the authority, operations are unchecked) or ifc (acquisition is unchecked, operations are checked against the stdio level)',
                // Repeating the option gives yargs an array. The compiler reads
                // the same flag and takes the last occurrence, so this does too:
                // otherwise a doubled flag leaves the two tools on different
                // models with nothing said about it.
                coerce: (v) => Array.isArray(v) ? v[v.length - 1] : v
            })
            .option(TroupeCliArg.Port, { type: 'number', describe: 'Network port for P2P communication' })
            .option(TroupeCliArg.File, { alias: 'f', type: 'string', describe: 'Path to the main troupe program file to execute' })
            .option(TroupeCliArg.RSpawn, { type: 'boolean', default: false, describe: 'Allow remote spawning of troupe processes' })
            .option(TroupeCliArg.Relay, { type: 'array', describe: 'Relay server multiaddress(es) for P2P connectivity' })
            .option(TroupeCliArg.RelayOnly, { type: 'boolean', default: false, describe: 'Disable DHT, mDNS, and bootstrap discovery; only use relay for peer connectivity' })
            .option(TroupeCliArg.NoP2pCircuit, {
                type: 'boolean',
                default: false,
                describe: 'Disable /p2p-circuit listen address (for testing NO_RESERVATION error handling)',
                coerce: () => {
                    // Handle the case where yargs interprets --no-p2p-circuit as negation
                    return process.argv.includes('--no-p2p-circuit');
                }
            })
            .option(TroupeCliArg.DisableRelay, { type: 'boolean', default: false, describe: 'Completely disable relay functionality (no circuit relay transport or connections)' })
            .option(TroupeCliArg.RelayFaultTolerance, {
                type: 'string',
                choices: ['fatal', 'no-fatal'],
                default: 'fatal',
                describe: 'Control whether relay connection failures are fatal (fatal) or non-fatal (no-fatal)'
            })
            .option(TroupeCliArg.NoColor, {
                type: 'boolean',
                default: false,
                describe: 'Disable colored output (also respects NO_COLOR env var)',
                coerce: (arg) => {
                    // Handle the case where yargs interprets --no-color as negation
                    // When --no-color is passed, arg will be false (negation of color)
                    // We want to return true when --no-color is present
                    return process.argv.includes('--no-color');
                }
            })
            .option(TroupeCliArg.LabelFormat, {
                type: 'string',
                choices: ['v1', 'v2', 'v2-full'],
                default: 'v1',
                describe: 'Label output format: v1 (tagset shorthand), v2 (DC with null elision), v2-full (DC fully explicit)'
            })
            .option(TroupeCliArg.SuppressLocalInfoMessage, {
                type: 'boolean',
                default: false,
                describe: 'Suppress info message when running in local-only mode'
            })
            .option(TroupeCliArg.SuppressMainThreadFinishedMessage, {
                type: 'boolean',
                default: false,
                describe: 'Suppress "Main thread finished" message on program completion'
            })
            .option(TroupeCliArg.Explain, {
                alias: 'e',
                type: 'boolean',
                default: false,
                describe: 'Enable detailed explanations for runtime errors (shows flowsTo checks and other diagnostics)'
            })
            .option(TroupeCliArg.Timeout, {
                type: 'number',
                default: 0,
                describe: 'Execution timeout in seconds (0 = no timeout)'
            })
            .option(TroupeCliArg.TimeoutExitCode, {
                type: 'number',
                default: 124,
                describe: 'Exit code to use when execution times out'
            })
            .option(TroupeCliArg.ResultSocket, {
                type: 'string',
                describe: 'Unix socket path for structured lifecycle messages (main thread result, process exit)'
            })
            .parseSync();

        if (rawArgs.f && !rawArgs.file) {
             rawArgs.file = String(rawArgs.f);
        }

        parsedArgs = rawArgs as ParsedArgs;

        // Validate relay-related options for consistency
        try {
            validateRelayOptions(parsedArgs);
        } catch (error) {
            handleCliError(error);
        }
    }
    return parsedArgs;
}

/**
 * Validates that relay-related CLI options are consistent with each other.
 * Throws an error if inconsistent options are detected.
 */
export function validateRelayOptions(args: ParsedArgs): void {
    const disableRelay = args[TroupeCliArg.DisableRelay];
    const relay = args[TroupeCliArg.Relay];
    const relayOnly = args[TroupeCliArg.RelayOnly];
    const noP2pCircuit = args[TroupeCliArg.NoP2pCircuit];

    if (disableRelay && relay && (Array.isArray(relay) ? relay.length > 0 : true)) {
        throw new CliValidationError("Inconsistent relay options: --disable-relay cannot be used with --relay");
    }
    if (disableRelay && relayOnly) {
        throw new CliValidationError("Inconsistent relay options: --disable-relay cannot be used with --relay-only");
    }
    if (relayOnly && noP2pCircuit) {
        throw new CliValidationError("Inconsistent relay options: --relay-only cannot be used with --no-p2p-circuit");
    }
} 