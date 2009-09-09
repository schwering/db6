# Executes release binaries (see rtest.sh for debug)
N=5
B=ttree
F=/home/chs/Disk/btree
L=OS_CACHE
E=ERR

for i in `seq 1 $N`
do
(	(date &&\
	build/release/./$B &&\
	date &&\
	ls -s --block-size=M $F &&\
	ls -s --block-size=G $F &&\
	echo "") ||\
	(echo "Crashed!" &&\
	date &&\
	echo "")
) >>$L 2>>$E
done

