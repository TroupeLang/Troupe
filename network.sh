#!/bin/bash

tmp=`mktemp`

$TROUPE/bin/troupec $1 --output=$tmp

if [ $? -eq 0 ]; then    
    shift 
    $TROUPE/rt/troupe "$tmp" "$@"  
    code=$?
    rm $tmp
    exit $code
else
    exit $?
fi
