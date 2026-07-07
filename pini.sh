#!/bin/sh

# Source shared environment setup
_TROUPE_CALLER_DIR="$(cd "$(dirname "$0")" && pwd)"
. "$_TROUPE_CALLER_DIR/scripts/troupe-common.sh"

tmp=`mktemp`.js

# Parse arguments (sets TROUPE_COMPILER_ARGS, TROUPE_RUNTIME_ARGS, TROUPE_PROGRAM_ARGS)
troupe_parse_args "$@"

$TROUPE_ROOT/bin/troupec $TROUPE_COMPILER_ARGS --output=$tmp
if [ $? -eq 0 ]; then
    eval "node --stack-trace-limit=1000 $TROUPE_ROOT/rt/built/troupe.mjs -f=$tmp --localonly --pini $TROUPE_RUNTIME_ARGS $TROUPE_PROGRAM_ARGS"
    exit_code=$?
    rm $tmp
    exit $exit_code
else
    exit $?
fi    

