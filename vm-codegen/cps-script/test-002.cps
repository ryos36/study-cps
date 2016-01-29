((:FIXH
  ((FACT0 (|sym11| K N RV)
    (:= (N 0) NIL
     ((:APP |sym11| (RV))
      (:- (N 1) (|sym17|)
       ((:* (N RV) (|sym16|) ((:APP FACT0 (|sym11| K |sym17| |sym16|)))))))))
   (FACT (|sym18| K N) (:APP FACT0 (|sym18| K N 1))))
  (:APP FACT (EXIT 10)))

 (:FIXH
  ((|:FACT0| (FACT0 |sym11| K N RV)
    (:= (N 0) NIL
     ((:RECORD-REF (|sym11| 0) (|k-sym2|) ((:APP |k-sym2| (|sym11| RV))))
      (:- (N 1) (|sym17|)
       ((:* (N RV) (|sym16|)
         ((:RECORD-REF (FACT0 0) (|k-sym3|)
           ((:APP |k-sym3| (FACT0 |sym11| K |sym17| |sym16|)))))))))))
   (|:FACT| (FACT |sym18| K N)
    (:RECORD-REF (FACT 1) (|k-sym4|)
     ((:RECORD-REF (|k-sym4| 1) (FACT0)
       ((:RECORD-REF (FACT0 0) (|k-sym5|)
         ((:APP |k-sym5| (FACT0 |sym18| K N 1))))))))))
  (:HEAP ((:LABEL :DUMMY) FACT0) (|k-sym0|)
   ((:HEAP ((:LABEL |:FACT0|) |k-sym0|) (FACT0)
     ((:HEAP ((:LABEL |:FACT|) |k-sym0|) (FACT)
       ((:RECORD-REF (FACT 0) (|k-sym6|)
         ((:APP |k-sym6| (FACT EXIT 10)))))))))))

    (:FIXH
     ((FACT (|sym23| K N)
       (:FIXH
        ((FACT0 (|sym24| K N RV)
          (:= (N 0) NIL
           ((:APP |sym24| (RV))
            (:- (N 1) (|sym30|)
             ((:* (N RV) (|sym29|)
               ((:APP FACT0 (|sym24| K |sym30| |sym29|))))))))))
        (:APP FACT0 (|sym23| K N 1)))))
     (:APP FACT (EXIT 10)))

    (:FIXH
     ((|:FACT| (FACT |sym23| K N)
       (:FIXH
        ((|:FACT0| (FACT0 |sym24| K N RV)
          (:= (N 0) NIL
           ((:RECORD-REF (|sym24| 0) (|k-sym14|) ((:APP |k-sym14| (|sym24| RV))))
            (:- (N 1) (|sym30|)
             ((:* (N RV) (|sym29|)
               ((:RECORD-REF (FACT0 0) (|k-sym15|)
                 ((:APP |k-sym15| (FACT0 |sym24| K |sym30| |sym29|))))))))))))
        (:HEAP ((:LABEL |:FACT0|)) (FACT0)
         ((:RECORD-REF (FACT0 0) (|k-sym16|)
           ((:APP |k-sym16| (FACT0 |sym23| K N 1)))))))))
     (:HEAP ((:LABEL |:FACT|)) (FACT)
      ((:RECORD-REF (FACT 0) (|k-sym17|) ((:APP |k-sym17| (FACT EXIT 10))))))))
