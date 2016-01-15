for i in `seq -f%03g 40`
do
    echo mv test$i.scm test-$i.scm
done
