((:FIXH
  ((FACT (|sym2| K N)
    (:FIXS ((|sym3| (|sym4|) (:APP |sym2| (|sym4|))))
     (:= (N 0) NIL
      ((:APP |sym3| (1))
       (:- (N 1) (|sym8|)
        ((:FIXS
          ((|sym6| (|sym7|) (:* (N |sym7|) (|sym5|) ((:APP |sym3| (|sym5|))))))
          (:APP FACT (|sym6| K |sym8|))))))))))
  (:FIXS ((|sym9| (|sym10|) (:APP EXIT (|sym10|)))) (:APP FACT (|sym9| 10))))
 
 (:FIXH
  ((FACT (|sym2| K N)
    (:= (N 0) NIL
     ((:APP |sym2| (1))
      (:- (N 1) (|sym8|)
       ((:FIXS
         ((|sym6| (|sym7|) (:* (N |sym7|) (|sym5|) ((:APP |sym2| (|sym5|))))))
         (:APP FACT (|sym6| K |sym8|)))))))))
  (:APP FACT (EXIT 10)))
 
 (:FIXH
  ((FACT0 (|sym11| K N RV)
    (:FIXS ((|sym12| (|sym13|) (:APP |sym11| (|sym13|))))
     (:= (N 0) NIL
      ((:APP |sym12| (RV))
       (:- (N 1) (|sym17|)
        ((:* (N RV) (|sym16|)
          ((:FIXS ((|sym14| (|sym15|) (:APP |sym12| (|sym15|))))
            (:APP FACT0 (|sym14| K |sym17| |sym16|)))))))))))
   (FACT (|sym18| K N)
    (:FIXS ((|sym19| (|sym20|) (:APP |sym18| (|sym20|))))
     (:APP FACT0 (|sym19| K N 1)))))
  (:FIXS ((|sym21| (|sym22|) (:APP EXIT (|sym22|)))) (:APP FACT (|sym21| 10))))
 
 (:FIXH
  ((FACT0 (|sym11| K N RV)
    (:= (N 0) NIL
     ((:APP |sym11| (RV))
      (:- (N 1) (|sym17|)
       ((:* (N RV) (|sym16|) ((:APP FACT0 (|sym11| K |sym17| |sym16|)))))))))
   (FACT (|sym18| K N) (:APP FACT0 (|sym18| K N 1))))
  (:APP FACT (EXIT 10)))
 
 (:FIXH
  ((FACT (|sym23| K N)
    (:FIXH
     ((FACT0 (|sym24| K N RV)
       (:FIXS ((|sym25| (|sym26|) (:APP |sym24| (|sym26|))))
        (:= (N 0) NIL
         ((:APP |sym25| (RV))
          (:- (N 1) (|sym30|)
           ((:* (N RV) (|sym29|)
             ((:FIXS ((|sym27| (|sym28|) (:APP |sym25| (|sym28|))))
               (:APP FACT0 (|sym27| K |sym30| |sym29|))))))))))))
     (:FIXS ((|sym31| (|sym32|) (:APP |sym23| (|sym32|))))
      (:APP FACT0 (|sym31| K N 1))))))
  (:FIXS ((|sym33| (|sym34|) (:APP EXIT (|sym34|)))) (:APP FACT (|sym33| 10))))
 
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
