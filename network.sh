#!/bin/sh

tmp=`mktemp`

$TROUPE/bin/troupec $1 --output=$tmp
shift 

if [ $? -eq 0 ]; then    
    $TROUPE/rt/troupe "$tmp" "$@"    
    code=$?
    rm $tmp
    exit $code
else
    exit $?
fi
