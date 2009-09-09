ITERATIONS=1
BINARY=build/release/./ttree
TREEFILE=.tmp/btree
DISKFILE=/dev/sda2
LOGFILE=OUT
ERRFILE=ERR

COUNT=2000
OFFSET=0

for i in `seq 1 $ITERATIONS`
do
(	(date &&\
	iostat >/dev/null
	${BINARY} ${TREEFILE} ${OFFSET} Insert,$COUNT &&\
	(iostat -c -d -k -x $DISKFILE | grep -v Linux | grep [:alnum:]) &&\
	${BINARY} ${TREEFILE} ${COUNT} Search,$COUNT &&\
	(iostat -c -d -k -x $DISKFILE | grep -v Linux | grep [:alnum:]) &&\
	${BINARY} ${TREEFILE} ${COUNT} Delete,$COUNT &&\
	(iostat -c -d -k -x $DISKFILE | grep -v Linux | grep [:alnum:]) &&\
	${BINARY} ${TREEFILE} ${OFFSET} Insert,$COUNT &&\
	(iostat -c -d -k -x $DISKFILE | grep -v Linux | grep [:alnum:]) &&\
	${BINARY} ${TREEFILE} ${COUNT} Search,$COUNT &&\
	(iostat -c -d -k -x $DISKFILE | grep -v Linux | grep [:alnum:]) &&\
	${BINARY} ${TREEFILE} ${COUNT} Check &&\
	date &&\
	ls -L -s --block-size=M ${TREEFILE} &&\
	ls -L -s --block-size=G ${TREEFILE} &&\
	echo "") ||\
	(echo "Crashed!" &&\
	date &&\
	echo "")
) >>$LOGFILE 2>>$ERRFILE
done

