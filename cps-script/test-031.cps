((:FIXH
  ((EVEN? (|sym2| X)
    (:FIXS ((|sym3| (|sym4|) (:APP |sym2| (|sym4|))))
     (:= (X 0) NIL
      ((:APP |sym3| (:|#T|))
       (:- (X 1) (|sym7|)
        ((:FIXS ((|sym5| (|sym6|) (:APP |sym3| (|sym6|))))
          (:APP ODD? (|sym5| |sym7|)))))))))
   (ODD? (|sym8| X)
    (:FIXS ((|sym9| (|sym10|) (:APP |sym8| (|sym10|))))
     (:= (X 0) NIL
      ((:APP |sym9| (:|#T|))
       (:- (X 1) (|sym13|)
        ((:FIXS ((|sym11| (|sym12|) (:APP |sym9| (|sym12|))))
          (:APP EVEN? (|sym11| |sym13|))))))))))
  (:FIXS ((|sym14| (|sym15|) (:APP EXIT (|sym15|))))
   (:APP EVEN? (|sym14| 997)))))
