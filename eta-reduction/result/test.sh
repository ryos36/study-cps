for i in `seq 13`
do
    F=`printf "%03d" $i`
    mv test$i.txt test-$F.txt
done
