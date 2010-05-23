#!/bin/sh
IO=map
BIN=bin/release/./tree
FILE=`cat .temp_path`btree
GEN=pseudorandom
C1=1k
# 1. Simple insertions and check
# 2. Some more insertions and check for both
# 3. Some mor insertions and check for all three
# 4. Delete of first insertions, check for effect
#    on all inserted elements (in fact, the first
#    two Searches are redundant with the last two)
# 5. Delete the last two blocks of insertions.
# 6. Check that there are no items left in the
#    tree (just for fun, check for four instead of
#    three blocks of insertions)
C="$BIN $IO $FILE $GEN 0\
        \
	Insert,$C1,Reset Check,1,Cont Stats,1,Cont\
	Insert,$C1,Reset Check,1,Cont Stats,1,Cont\
	Insert,$C1,Reset Check,1,Cont Stats,1,Cont\
	"
echo $C
$C

