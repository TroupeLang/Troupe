#!/bin/sh

export f1=$1
export f2=$2

bash -c "diff <(head -n 100 ${f1})  <(head -n 100 ${f2}) -u -I '^[[:digit:]]\{4\}-[[:digit:]]\{2\}-[[:digit:]]\{2\}T.*\[RTM\]' -I '[0-9a-f]\{8\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{12\}'"

