#cmd="rm -f .tmp/btree && ./testdouble3.sh 2>&1"
cmd=$*
out=""
outf=".out"
while [ ! -n "$out" ]
do
        echo "Executing $cmd"
        sh -c "$cmd" 2>&1 >>"$outf"
        retval=$?
        echo "Return value: $retval"
        echo "Last 50 lines of output:"
        tail -n 50 "$outf"
        if [ "$retval" -gt 0 ]
        then
                echo "Unsuccessful exit"
                exit
        fi
        out=`cat "$outf" | grep Exception`
done

