((:FIXH
  ((|:FIBO-2| (FIBO-2 |sym2| N A-1 A-2)
    (:= (N 0) NIL
     ((:RECORD-REF (|sym2| 0) (|k-sym1|) ((:APP |k-sym1| (|sym2| A-1))))
      (:- (N 1) (|sym8|)
       ((:+ (A-1 A-2) (|sym7|)
         ((:RECORD-REF (FIBO-2 0) (|k-sym2|)
           ((:APP |k-sym2| (FIBO-2 |sym2| |sym8| |sym7| A-1)))))))))))
   (|:FIBO| (FIBO |sym9| N)
    (:RECORD-OFFS (FIBO 1) (FIBO-2)
     ((:RECORD-REF (FIBO-2 0) (|k-sym3|)
       ((:APP |k-sym3| (FIBO-2 |sym9| N 1 0))))))))
  (:HEAP ((:LABEL |:FIBO|) (:LABEL |:FIBO-2|)) (FIBO)
   ((:RECORD-REF (FIBO 0) (|k-sym4|) ((:APP |k-sym4| (FIBO EXIT 8))))))))
