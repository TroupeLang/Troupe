#!/bin/sh

tmp=`mktemp`
tmpprocessed=`mktemp`
$TROUPE/bin/troupec $1 --output=$tmp
if [ $? -eq 0 ]; then
    node --prof --no-logfile-per-isolate --stack-trace-limit=1000 $TROUPE/rt/built/troupe.js  -f=$tmp --localonly #  --debug
    rm $tmp
    node --prof-process v8.log > $tmpprocessed.txt
    atom $tmpprocessed.txt
else 
    exit $?
fi    

