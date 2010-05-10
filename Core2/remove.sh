out=""
while [ ! -n "$out" ]
do
        echo "Executing $cmd"
        if [ -f .tmp/btree ]
        then
                unlink .tmp/btree.bak
                link .tmp/btree .tmp/btree.bak
                unlink .tmp/btree
        fi
        sleep 1
        out=`$cmd 2>&1`
        echo $out
        out=`echo "$out" | grep Exception`
done

