#!/bin/sh
PID=$1
DEV=$2
#pidstat -p $PID -d -r -u -w
iostat -c -d -k -x -t $DEV

