#cmd="rm -f .tmp/btree && ./testdouble3.sh 2>&1"
cmd=$*
out=""
outf=".out"
while [ ! -n "$out" ]
do
        echo "Executing $cmd"
        rm -f .tmp/btree
        $cmd 2>&1 >"$outf"
        cat "$outf"
        out=`cat "$outf" | grep Exception`
done

