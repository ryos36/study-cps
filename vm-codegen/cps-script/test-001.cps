((:FIXH
  ((|:EVEN?| (EVEN? |sym2| X)
    (:RECORD-OFFS (EVEN? 1) (ODD?)
     ((:= (X 0) NIL
       ((:RECORD-REF (|sym2| 0) (|k-sym1|) ((:APP |k-sym1| (|sym2| :|#T|))))
        (:- (X 1) (|sym7|)
         ((:RECORD-REF (ODD? 0) (|k-sym2|)
           ((:APP |k-sym2| (ODD? |sym2| |sym7|)))))))))))
   (|:ODD?| (ODD? |sym8| X)
    (:RECORD-REF (ODD? 1) (EVEN?)
     ((:= (X 0) NIL
       ((:RECORD-REF (|sym8| 0) (|k-sym3|) ((:APP |k-sym3| (|sym8| :|#T|))))
        (:- (X 1) (|sym13|)
         ((:RECORD-REF (EVEN? 0) (|k-sym4|)
           ((:APP |k-sym4| (EVEN? |sym8| |sym13|))))))))))))
  (:HEAP ((:LABEL |:EVEN?|) (:LABEL |:ODD?|) :|#F|) (EVEN?)
   ((:RECORD-SET! (EVEN? 2 EVEN?) NIL
     ((:RECORD-REF (EVEN? 0) (|k-sym5|) ((:APP |k-sym5| (EVEN? EXIT 997))))))))))
