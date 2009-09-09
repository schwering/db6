BIN=build/debug/./vttree
FILE=.tmp/Disk/Volatile
C1=50k
$BIN $FILE\
	0 Insert,$C1,True\
	Search,$C1,True\
	Insert,$C1,False\
	Search,$C1,True\
	Search,$C1,False\
	Insert,$C1,False\
	Search,$C1,True\
	Search,$C1,False\
	Search,$C1,False

