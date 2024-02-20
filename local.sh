#!/bin/sh

tmp=`mktemp`.js

$TROUPE/bin/troupec $@ --output=$tmp

if [ $? -eq 0 ]; then
    node --stack-trace-limit=1000 $TROUPE/rt/built/troupe.mjs  -f=$tmp --localonly  #--debug
    rm $tmp
else 
    exit $?
fi    

