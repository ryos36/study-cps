((:FIXH
  ((FACT0 (|sym11| K N RV)
    (:= (N 0) NIL
     ((:APP |sym11| (RV))
      (:- (N 1) (|sym17|)
       ((:* (N RV) (|sym16|) 
         ((:APP FACT0 (|sym11| K |sym17| |sym16|)))))))))
   (FACT (|sym18| K N) (:APP FACT0 (|sym18| K N 1))))
  (:APP FACT (EXIT 10)))
 
 
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
  (:APP FACT (EXIT 10))))
