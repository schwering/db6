#cmd="rm -f .tmp/btree && ./testdouble3.sh 2>&1"
cmd=$*
out=""
while [ ! -n "$out" ]
do
        echo "Executing $cmd"
        rm -f .tmp/btree
        out=`$cmd 2>&1`
        echo $out
        out=`echo "$out" | grep Exception`
done

