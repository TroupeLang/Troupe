#!/bin/sh
# Troupe common utilities for shell scripts that compile and run programs
# Includes environment setup and argument parsing
#
# Usage from root-level scripts:
#   _TROUPE_CALLER_DIR="$(cd "$(dirname "$0")" && pwd)"
#   . "$_TROUPE_CALLER_DIR/scripts/troupe-common.sh"

# Source environment setup first
if [ -n "${BASH_SOURCE:-}" ]; then
    . "$(dirname "${BASH_SOURCE[0]}")/troupe-env.sh"
elif [ -n "${_TROUPE_CALLER_DIR:-}" ]; then
    . "$_TROUPE_CALLER_DIR/scripts/troupe-env.sh"
else
    . "$(dirname "$0")/troupe-env.sh"
fi

# Shared argument parsing function for compile-and-run scripts
# Separates compiler args, runtime args, and program args (after --)
# Sets: TROUPE_COMPILER_ARGS, TROUPE_RUNTIME_ARGS, TROUPE_PROGRAM_ARGS
# Usage: troupe_parse_args "$@"
troupe_parse_args() {
    TROUPE_COMPILER_ARGS=""
    TROUPE_RUNTIME_ARGS=""
    TROUPE_PROGRAM_ARGS=""
    _seen_separator=false
    _expect_runtime_value=false

    for arg in "$@"; do
        if [ "$_seen_separator" = true ]; then
            TROUPE_PROGRAM_ARGS="$TROUPE_PROGRAM_ARGS \"$arg\""
        elif [ "$arg" = "--" ]; then
            _seen_separator=true
            TROUPE_PROGRAM_ARGS="--"
        elif [ "$_expect_runtime_value" = true ]; then
            # This arg is the value for a runtime option
            TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS \"$arg\""
            _expect_runtime_value=false
        else
            case "$arg" in
                # Runtime boolean options (no value expected)
                --debug|-d|--debugsandbox|--debugmailbox|--debugp2p|--debugquarantine)
                    TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS $arg"
                    ;;
                --pini|--nmifc|--showStack|-ss|--rspawn|--localonly|-l|--persist|-P)
                    TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS $arg"
                    ;;
                --no-color|--no-nmifc|--suppress-local-info-message|--suppress-main-thread-finished-message|--explain|-e|--relay-only|--disable-relay|--no-p2p-circuit)
                    TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS $arg"
                    ;;
                # Runtime options with embedded value (--option=value). Quoted
                # on the way out: the value reaches the runtime through an
                # eval, and a label level spells `<`, `>` and `;`.
                --trustmap=*|--id=*|--aliases=*|--stdiolev=*|--io-root=*|--port=*|--relay=*|--label-format=*|--relay-fault-tolerance=*|--timeout=*|--timeout-exit-code=*)
                    TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS \"$arg\""
                    ;;
                # Runtime options expecting a separate value
                --trustmap|-tm|--id|-i|--aliases|-a|--stdiolev|--io-root|--port|--relay|--label-format|--relay-fault-tolerance|--timeout|--timeout-exit-code)
                    TROUPE_RUNTIME_ARGS="$TROUPE_RUNTIME_ARGS $arg"
                    _expect_runtime_value=true
                    ;;
                *)
                    TROUPE_COMPILER_ARGS="$TROUPE_COMPILER_ARGS $arg"
                    ;;
            esac
        fi
    done
    unset _seen_separator _expect_runtime_value
}
