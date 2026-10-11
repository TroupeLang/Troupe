// The runtime's options record and the accessor every module reads it through.
//
// This module is platform-neutral on purpose: it holds no parser and imports
// neither yargs nor process. A host supplies the record -- Node by registering
// its command-line parser (hosts/node/cliArgs.mts), another host by seeding the
// record directly with setCliArgs(). Registration or seeding must happen before
// any module that reads options at import time is loaded.

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
    /** The program's own arguments: on a command line, those after `--`. */
    programArgs?: string[];
    [key: string]: any;
}

let parsedArgs: ParsedArgs | null = null;
let argsParser: (() => ParsedArgs) | null = null;

/**
 * Options as they stand with nothing supplied: every documented default, and
 * nothing else. Used when no host has registered a parser or seeded a record.
 */
export function defaultCliArgs(): ParsedArgs {
    return {
        [TroupeCliArg.Debug]: false,
        [TroupeCliArg.DebugSandbox]: false,
        [TroupeCliArg.DebugMailbox]: false,
        [TroupeCliArg.DebugP2p]: false,
        [TroupeCliArg.DebugQuarantine]: false,
        [TroupeCliArg.Pini]: false,
        [TroupeCliArg.Nmifc]: true,
        [TroupeCliArg.ShowStack]: false,
        [TroupeCliArg.LocalOnly]: false,
        [TroupeCliArg.Persist]: false,
        [TroupeCliArg.RSpawn]: false,
        [TroupeCliArg.RelayOnly]: false,
        [TroupeCliArg.NoP2pCircuit]: false,
        [TroupeCliArg.DisableRelay]: false,
        [TroupeCliArg.RelayFaultTolerance]: 'fatal',
        [TroupeCliArg.NoColor]: false,
        [TroupeCliArg.LabelFormat]: 'v1',
        [TroupeCliArg.SuppressLocalInfoMessage]: false,
        [TroupeCliArg.SuppressMainThreadFinishedMessage]: false,
        [TroupeCliArg.Explain]: false,
        [TroupeCliArg.Timeout]: 0,
        [TroupeCliArg.TimeoutExitCode]: 124,
        programArgs: [],
    };
}

/**
 * Registers the function that produces the options record on first read.
 * Node's entry point registers its yargs parser here.
 */
export function registerCliArgsParser(parser: () => ParsedArgs): void {
    argsParser = parser;
}

/**
 * Supplies the options record directly, for hosts that have no command line.
 */
export function setCliArgs(args: ParsedArgs): void {
    parsedArgs = args;
}

export function getCliArgs(): ParsedArgs {
    if (!parsedArgs) {
        parsedArgs = argsParser ? argsParser() : defaultCliArgs();
    }
    return parsedArgs;
}
