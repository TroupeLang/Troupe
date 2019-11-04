#!/bin/sh

tmp=`mktemp`

$TROUPE/bin/troupec $1 --output=$tmp
if [ $? -eq 0 ]; then
    node --stack-trace-limit=1000 $TROUPE/rt/built/troupe.js  -f=$tmp --localonly #  --debug
    rm $tmp
else 
    exit $?
fi    

