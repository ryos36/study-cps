((:FIXH
  ((|:FACT0| (FACT0 |sym11| K N RV)
    (:= (N 0) NIL
     ((:RECORD-REF (|sym11| 0) (|k-sym1|) ((:APP |k-sym1| (|sym11| RV))))
      (:- (N 1) (|sym17|)
       ((:* (N RV) (|sym16|)
         ((:RECORD-REF (FACT0 0) (|k-sym2|)
           ((:APP |k-sym2| (FACT0 |sym11| K |sym17| |sym16|)))))))))))
   (|:FACT| (FACT |sym18| K N)
    (:RECORD-OFFS (FACT 1) (FACT0)
     ((:RECORD-REF (FACT0 0) (|k-sym3|)
       ((:APP |k-sym3| (FACT0 |sym18| K N 1))))))))
  (:HEAP ((:LABEL |:FACT|) (:LABEL |:FACT0|)) (FACT)
   ((:RECORD-REF (FACT 0) (|k-sym4|) ((:APP |k-sym4| (FACT EXIT 10)))))))

 (:FIXH
  ((|:FACT| (FACT |sym23| K N)
    (:FIXH
     ((|:FACT0| (FACT0 |sym24| K N RV)
       (:= (N 0) NIL
        ((:RECORD-REF (|sym24| 0) (|k-sym5|) ((:APP |k-sym5| (|sym24| RV))))
         (:- (N 1) (|sym30|)
          ((:* (N RV) (|sym29|)
            ((:RECORD-REF (FACT0 0) (|k-sym6|)
              ((:APP |k-sym6| (FACT0 |sym24| K |sym30| |sym29|))))))))))))
     (:HEAP ((:LABEL |:FACT0|)) (FACT0)
      ((:RECORD-REF (FACT0 0) (|k-sym7|)
        ((:APP |k-sym7| (FACT0 |sym23| K N 1)))))))))
  (:HEAP ((:LABEL |:FACT|)) (FACT)
   ((:RECORD-REF (FACT 0) (|k-sym8|) ((:APP |k-sym8| (FACT EXIT 10))))))))
