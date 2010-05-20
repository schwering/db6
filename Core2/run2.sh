#!/bin/sh
IO=map
BIN=bin/release/./ttree
FILE=`cat .temp_path`btree
GEN=urls
C1=100k
# 1. Simple insertions twice and check
# 2. Some more insertions and check for both
# 3. Some mor insertions and check for all three
# 4. Delete of first double insertions, check for
#    effect on all inserted elements (in fact, the
#    first two Searches are redundant with the last two)
# 5. Delete the last two blocks of insertions.
# 6. Check that there are no items left in the
#    tree (just for fun, check for four instead of
#    three blocks of insertions)
C="$BIN $IO $FILE $GEN 0\
        \
	Insert,$C1,Cont\
	Insert,$C1,Reset\
	Search,$C1,Reset\
        \
	Insert,$C1,Cont\
	Search,$C1,Reset\
	Search,$C1,Cont\
        \
	Insert,$C1,Cont\
	Search,$C1,Reset\
	Search,$C1,Cont\
	Search,$C1,Cont\
        \
        Delete,$C1,Reset\
        Delete,$C1,Reset\
	Antisearch,$C1,Reset\
	Search,$C1,Cont\
	Search,$C1,Cont\
        \
	Insert,$C1,Reset\
	Search,$C1,Reset\
	Search,$C1,Cont\
	Search,$C1,Cont"
echo $C
$C
#C="$BIN $IO $FILE $GEN 0 Check,1,Reset"
#echo $C
#$C
C="$BIN $IO $FILE $GEN 0 Stats,1,Reset"
echo $C
$C
