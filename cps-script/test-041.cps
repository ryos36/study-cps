((:HEAP (19201738) (|sym45|)
 ((:FIXH
   ((CLEAR-BIT0 (|sym2| X)
     (:>> (X 1) (|sym4|) ((:<< (|sym4| 1) (|sym3|) ((:APP |sym2| (|sym3|)))))))
    (GET-BIT0 (|sym5| X)
     (:FIXS
      ((|sym7| (|sym8|) (:- (X |sym8|) (|sym6|) ((:APP |sym5| (|sym6|))))))
      (:APP CLEAR-BIT0 (|sym7| X))))
    (XOR-BIT (|sym9| B0 B1)
     (:= (B0 B1) NIL ((:APP |sym9| (0)) (:APP |sym9| (1)))))
    (XOR0 (|sym12| BIT-N N X0 X1 RV)
     (:= (0 BIT-N) NIL
      ((:APP |sym12| (RV))
       (:FIXS
        ((|sym27| (|sym28|)
          (:FIXS
           ((|sym25| (|sym26|)
             (:- (BIT-N 1) (|sym24|)
              ((:+ (N 1) (|sym23|)
                ((:>> (X0 1) (|sym22|)
                  ((:>> (X1 1) (|sym21|)
                    ((:FIXS
                      ((|sym19| (|sym20|)
                        (:<< (|sym20| N) (|sym18|)
                         ((:+ (RV |sym18|) (|sym17|)
                           ((:APP XOR0
                             (|sym12| |sym24| |sym23| |sym22| |sym21|
                              |sym17|))))))))
                      (:APP XOR-BIT (|sym19| |sym28| |sym26|)))))))))))))
           (:APP GET-BIT0 (|sym25| X1)))))
        (:APP GET-BIT0 (|sym27| X0))))))
    (XOR (|sym29| BIT-N X0 X1) (:APP XOR0 (|sym29| BIT-N 0 X0 X1 0)))
    (RAND (|sym32|)
     (:RECORD-REF (|sym45| 0) (|sym42|)
      ((:<< (|sym42| 13) (|sym41|)
        ((:FIXS
          ((|sym39| (|sym40|)
            (:>> (|sym40| 17) (|sym38|)
             ((:FIXS
               ((|sym36| (|sym37|)
                 (:<< (|sym37| 15) (|sym35|)
                  ((:FIXS
                    ((|sym33| (|sym34|)
                      (:RECORD-SET! (|sym34| 0 |sym45|) NIL
                       ((:APP |sym32| (|sym34|))))))
                    (:APP XOR (|sym33| 32 |sym37| |sym35|)))))))
               (:APP XOR (|sym36| 32 |sym40| |sym38|)))))))
          (:APP XOR (|sym39| 32 |sym42| |sym41|)))))))))
   (:FIXS ((|sym43| (|sym44|) (:EXIT (|sym44|) NIL NIL)))
    (:APP RAND (|sym43|)))))))
