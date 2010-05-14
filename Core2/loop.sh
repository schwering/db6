#cmd="rm -f .tmp/btree && ./testdouble3.sh 2>&1"
cmd=$*
out=""
outf=".out"
while [ ! -n "$out" ]
do
        echo "Executing $cmd"
        rm -f .tmp/btree
        $cmd 2>&1 >"$outf"
        retval=$?
        echo $retval
        cat "$outf"
        if [ "$retval" -gt 0 ]
        then
                echo "Unsuccessful exit"
                exit
        fi
        out=`cat "$outf" | grep Exception`
done

