#!/bin/bash
cc -Wall cat.c -o cat || exit
cc -Wall -DCHUNK=4096 -I/usr/incude/ -L/usr/lib/ -lz zpipe.c -o zpipe || exit
for i in {0..30000}
do
	echo -n "$i: "
	 (./cat ../Core/.tmp/btree $i 4096 | ./zpipe | wc -c) 2>/dev/null || exit
done

