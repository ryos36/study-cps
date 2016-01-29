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
 (:APP RAND (|sym43|))))))

(:HEAP (19201738) (|sym45|)
 ((:FIXH
   ((|:CLEAR-BIT0| (CLEAR-BIT0 |sym2| X)
     (:>> (X 1) (|sym4|)
      ((:<< (|sym4| 1) (|sym3|)
        ((:RECORD-REF (|sym2| 0) (|sym2|) ((:APP |sym2| (|sym2| |sym3|)))))))))
    (|:GET-BIT0| (GET-BIT0 |sym5| X)
     (:RECORD-REF (GET-BIT0 1) (|sym3|)
      ((:RECORD-REF (|sym3| 6) (CLEAR-BIT0)
        ((:FIXS
          ((|:sym7| (|sym7| |sym8|)
            (:RECORD-REF (|sym7| 2) (X)
             ((:RECORD-REF (|sym7| 1) (|sym5|)
               ((:POP (3) NIL
                 ((:- (X |sym8|) (|sym6|)
                   ((:RECORD-REF (|sym5| 0) (|sym4|)
                     ((:APP |sym4| (|sym5| |sym6|))))))))))))))
          (:STACK ((:LABEL |:sym7|) |sym5| X) (|sym7|)
           ((:RECORD-REF (CLEAR-BIT0 0) (|sym5|)
             ((:APP |sym5| (CLEAR-BIT0 |sym7| X))))))))))))
    (|:XOR-BIT| (XOR-BIT |sym9| B0 B1)
     (:= (B0 B1) NIL
      ((:RECORD-REF (|sym9| 0) (|sym7|) ((:APP |sym7| (|sym9| 0))))
       (:RECORD-REF (|sym9| 0) (|sym8|) ((:APP |sym8| (|sym9| 1)))))))
    (|:XOR0| (XOR0 |sym12| BIT-N N X0 X1 RV)
     (:RECORD-REF (XOR0 1) (|sym9|)
      ((:RECORD-REF (|sym9| 5) (XOR-BIT)
        ((:RECORD-REF (|sym9| 4) (GET-BIT0)
          ((:= (0 BIT-N) NIL
            ((:RECORD-REF (|sym12| 0) (|sym10|) ((:APP |sym10| (|sym12| RV))))
             (:FIXS
              ((|:sym27| (|sym27| |sym28|)
                (:FIXS
                 ((|:sym25| (|sym25| |sym26|)
                   (:RECORD-REF (|sym25| 4) (BIT-N)
                    ((:RECORD-REF (|sym25| 3) (N)
                      ((:RECORD-REF (|sym25| 2) (X0)
                        ((:RECORD-REF (|sym25| 1) (X1)
                          ((:POP (5) NIL
                            ((:- (BIT-N 1) (|sym24|)
                              ((:+ (N 1) (|sym23|)
                                ((:>> (X0 1) (|sym22|)
                                  ((:>> (X1 1) (|sym21|)
                                    ((:FIXS
                                      ((|:sym19| (|sym19| |sym20|)
                                        (:RECORD-REF (|sym19| 8) (N)
                                         ((:RECORD-REF (|sym19| 7) (RV)
                                           ((:RECORD-REF (|sym19| 6) (XOR0)
                                             ((:RECORD-REF (|sym19| 5)
                                               (|sym12|)
                                               ((:RECORD-REF (|sym19| 4)
                                                 (|sym24|)
                                                 ((:RECORD-REF (|sym19| 3)
                                                   (|sym23|)
                                                   ((:RECORD-REF (|sym19| 2)
                                                     (|sym22|)
                                                     ((:RECORD-REF (|sym19| 1)
                                                       (|sym21|)
                                                       ((:POP (9) NIL
                                                         ((:<< (|sym20| N)
                                                           (|sym18|)
                                                           ((:+ (RV |sym18|)
                                                             (|sym17|)
                                                             ((:RECORD-REF
                                                               (XOR0 0)
                                                               (|sym11|)
                                                               ((:APP |sym11|
                                                                 (XOR0 |sym12|
                                                                  |sym24|
                                                                  |sym23|
                                                                  |sym22|
                                                                  |sym21|
                                                                  |sym17|))))))))))))))))))))))))))))
                                                                  (:STACK
                                                                   ((:LABEL |:sym19|) |sym21| |sym22|
                                                                    |sym23| |sym24| |sym12| XOR0 RV N)
                                                                   (|sym19|)
                                                                   ((:RECORD-REF (XOR-BIT 0) (|sym12|)
                                                                     ((:APP |sym12|
                                                                       (XOR-BIT |sym19| |sym28|
                                                                        |sym26|)))))))))))))))))))))))))))
    (:STACK ((:LABEL |:sym25|) X1 X0 N BIT-N) (|sym25|)
     ((:RECORD-REF (GET-BIT0 0) (|sym13|)
       ((:APP |sym13| (GET-BIT0 |sym25| X1)))))))))
(:RECORD-REF (GET-BIT0 0) (|sym14|)
 ((:APP |sym14| (GET-BIT0 |sym27| X0))))))))))))))
    (|:XOR| (XOR |sym29| BIT-N X0 X1)
     (:RECORD-REF (XOR 1) (|sym15|)
      ((:RECORD-REF (|sym15| 3) (XOR0)
        ((:RECORD-REF (XOR0 0) (|sym16|)
          ((:APP |sym16| (XOR0 |sym29| BIT-N 0 X0 X1 0)))))))))
    (|:RAND| (RAND |sym32|)
     (:RECORD-REF (RAND 1) (|sym17|)
      ((:RECORD-REF (|sym17| 2) (|sym45|)
        ((:RECORD-REF (|sym17| 1) (XOR)
          ((:RECORD-REF (|sym45| 0) (|sym42|)
            ((:<< (|sym42| 13) (|sym41|)
              ((:FIXS
                ((|:sym39| (|sym39| |sym40|)
                  (:>> (|sym40| 17) (|sym38|)
                   ((:FIXS
                     ((|:sym36| (|sym36| |sym37|)
                       (:<< (|sym37| 15) (|sym35|)
                        ((:FIXS
                          ((|:sym33| (|sym33| |sym34|)
                            (:RECORD-REF (|sym33| 2) (|sym45|)
                             ((:RECORD-REF (|sym33| 1) (|sym32|)
                               ((:POP (3) NIL
                                 ((:RECORD-SET! (|sym34| 0 |sym45|) NIL
                                   ((:RECORD-REF (|sym32| 0) (|sym18|)
                                     ((:APP |sym18|
                                       (|sym32| |sym34|))))))))))))))
                          (:STACK ((:LABEL |:sym33|) |sym32| |sym45|) (|sym33|)
                           ((:RECORD-REF (XOR 0) (|sym19|)
                             ((:APP |sym19|
                               (XOR |sym33| 32 |sym37| |sym35|)))))))))))
                     (:RECORD-REF (XOR 0) (|sym20|)
                      ((:APP |sym20| (XOR |sym36| 32 |sym40| |sym38|)))))))))
(:RECORD-REF (XOR 0) (|sym21|)
 ((:APP |sym21|
   (XOR |sym39| 32 |sym42| |sym41|)))))))))))))))))
    (:HEAP ((:LABEL :DUMMY) XOR |sym45| XOR0 GET-BIT0 XOR-BIT CLEAR-BIT0)
     (|sym0|)
     ((:HEAP ((:LABEL |:CLEAR-BIT0|) |sym0|) (CLEAR-BIT0)
       ((:HEAP ((:LABEL |:GET-BIT0|) |sym0|) (GET-BIT0)
         ((:HEAP ((:LABEL |:XOR-BIT|) |sym0|) (XOR-BIT)
           ((:HEAP ((:LABEL |:XOR0|) |sym0|) (XOR0)
             ((:HEAP ((:LABEL |:XOR|) |sym0|) (XOR)
               ((:HEAP ((:LABEL |:RAND|) |sym0|) (RAND)
                 ((:FIXS
                   ((|:sym43| (|sym43| |sym44|) (:EXIT (|sym44|) NIL NIL)))
                   (:RECORD-REF (RAND 0) (|sym22|)
                    ((:APP |sym22| (RAND |sym43|)))))))))))))))))))))))
