((:HEAP (19201738) (|sym45|)
  ((:FIXH
    ((|:CLEAR-BIT0| (CLEAR-BIT0 |sym2| X)
      (:>> (X 1) (|sym4|)
       ((:<< (|sym4| 1) (|sym3|)
         ((:RECORD-REF (|sym2| 0) (|k-sym1|)
           ((:APP |k-sym1| (|sym2| |sym3|)))))))))
     (|:GET-BIT0| (GET-BIT0 |sym5| X)
      (:RECORD-OFFS (GET-BIT0 1) (CLEAR-BIT0)
       ((:FIXS
         ((|:sym7| (|sym7| |sym8|)
           (:RECORD-REF (|sym7| 2) (|sym5|)
            ((:RECORD-REF (|sym7| 1) (X)
              ((:POP (3) NIL
                ((:- (X |sym8|) (|sym6|)
                  ((:RECORD-REF (|sym5| 0) (|k-sym2|)
                    ((:APP |k-sym2| (|sym5| |sym6|))))))))))))))
         (:STACK ((:LABEL |:sym7|) X |sym5|) (|sym7|)
          ((:RECORD-REF (CLEAR-BIT0 0) (|k-sym3|)
            ((:APP |k-sym3| (CLEAR-BIT0 |sym7| X))))))))))
     (|:XOR-BIT| (XOR-BIT |sym9| B0 B1)
      (:= (B0 B1) NIL
       ((:RECORD-REF (|sym9| 0) (|k-sym4|) ((:APP |k-sym4| (|sym9| 0))))
        (:RECORD-REF (|sym9| 0) (|k-sym5|) ((:APP |k-sym5| (|sym9| 1)))))))
    (|:XOR0| (XOR0 |sym12| BIT-N N X0 X1 RV)
     (:RECORD-OFFS (XOR0 1) (GET-BIT0)
      ((:RECORD-OFFS (XOR0 3) (XOR-BIT)
        ((:= (0 BIT-N) NIL
          ((:RECORD-REF (|sym12| 0) (|k-sym6|) ((:APP |k-sym6| (|sym12| RV))))
           (:FIXS
            ((|:sym27| (|sym27| |sym28|)
              (:RECORD-REF (|sym27| 9) (GET-BIT0)
               ((:RECORD-REF (|sym27| 8) (XOR-BIT)
                 ((:RECORD-REF (|sym27| 7) (|sym12|)
                   ((:RECORD-REF (|sym27| 6) (XOR0)
                     ((:RECORD-REF (|sym27| 5) (RV)
                       ((:RECORD-REF (|sym27| 4) (X1)
                         ((:RECORD-REF (|sym27| 3) (X0)
                           ((:RECORD-REF (|sym27| 2) (N)
                             ((:RECORD-REF (|sym27| 1) (BIT-N)
                               ((:POP (10) NIL
                                 ((:FIXS
                                   ((|:sym25| (|sym25| |sym26|)
                                     (:RECORD-REF (|sym25| 9) (XOR-BIT)
                                      ((:RECORD-REF (|sym25| 8) (|sym28|)
                                        ((:RECORD-REF (|sym25| 7) (|sym12|)
                                          ((:RECORD-REF (|sym25| 6) (XOR0)
                                            ((:RECORD-REF (|sym25| 5) (RV)
                                              ((:RECORD-REF (|sym25| 4) (X1)
                                                ((:RECORD-REF (|sym25| 3) (X0)
                                                  ((:RECORD-REF (|sym25| 2) (N)
                                                    ((:RECORD-REF (|sym25| 1)
                                                      (BIT-N)
                                                      ((:POP (10) NIL
                                                        ((:- (BIT-N 1)
                                                          (|sym24|)
                                                          ((:+ (N 1) (|sym23|)
                                                            ((:>> (X0 1)
                                                              (|sym22|)
                                                              ((:>> (X1 1)
                                                                (|sym21|)
                                                                ((:FIXS
                                                                  ((|:sym19|
                                                                    (|sym19|
                                                                     |sym20|)
                                                                    (:RECORD-REF
                                                                     (|sym19|
                                                                      8)
                                                                     (|sym21|)
                                                                     ((:RECORD-REF
                                                                       (|sym19|
                                                                        7)
                                                                       (|sym22|)
                                                                       ((:RECORD-REF
                                                                         (|sym19|
                                                                          6)
                                                                         (|sym23|)
                                                                         ((:RECORD-REF
                                                                           (|sym19|
                                                                            5)
                                                                           (|sym24|)
                                                                           ((:RECORD-REF
                                                                             (|sym19|
                                                                              4)
                                                                             (|sym12|)
                                                                             ((:RECORD-REF
                                                                               (|sym19|
                                                                                3)
                                                                               (XOR0)
                                                                               ((:RECORD-REF
                                                                                 (|sym19|
                                                                                  2)
                                                                                 (RV)
                                                                                 ((:RECORD-REF
                                                                                   (|sym19|
                                                                                    1)
                                                                                   (N)
                                                                                   ((:POP
                                                                                     (9)
                                                                                     NIL
                                                                                     ((:<<
                                                                                       (|sym20|
                                                                                        N)
                                                                                       (|sym18|)
                                                                                       ((:+
                                                                                         (RV
                                                                                          |sym18|)
                                                                                         (|sym17|)
                                                                                         ((:RECORD-REF
                                                                                           (XOR0
                                                                                            0)
                                                                                           (|k-sym7|)
                                                                                           ((:APP
                                                                                             |k-sym7|
                                                                                             (XOR0
                                                                                              |sym12|
                                                                                              |sym24|
                                                                                              |sym23|
                                                                                              |sym22|
                                                                                              |sym21|
                                                                                              |sym17|))))))))))))))))))))))))))))
                                                                                              (:STACK
                                                                                               ((:LABEL
                                                                                                 |:sym19|)
                                                                                                N RV XOR0
                                                                                                |sym12|
                                                                                                |sym24|
                                                                                                |sym23|
                                                                                                |sym22|
                                                                                                |sym21|)
                                                                                               (|sym19|)
                                                                                               ((:RECORD-REF
                                                                                                 (XOR-BIT
                                                                                                  0)
                                                                                                 (|k-sym8|)
                                                                                                 ((:APP
                                                                                                   |k-sym8|
                                                                                                   (XOR-BIT
                                                                                                    |sym19|
                                                                                                    |sym28|
                                                                                                    |sym26|)))))))))))))))))))))))))))))))))))))
                                                                                                    (:STACK
                                                                                                     ((:LABEL |:sym25|) BIT-N N X0 X1 RV XOR0
                                                                                                      |sym12| |sym28| XOR-BIT)
                                                                                                     (|sym25|)
                                                                                                     ((:RECORD-REF (GET-BIT0 0) (|k-sym9|)
                                                                                                       ((:APP |k-sym9|
                                                                                                         (GET-BIT0 |sym25|
                                                                                                          X1)))))))))))))))))))))))))))))
    (:STACK
     ((:LABEL |:sym27|) BIT-N N X0 X1 RV XOR0 |sym12| XOR-BIT GET-BIT0)
     (|sym27|)
     ((:RECORD-REF (GET-BIT0 0) (|k-sym10|)
       ((:APP |k-sym10| (GET-BIT0 |sym27| X0))))))))))))))
    (|:XOR| (XOR |sym29| BIT-N X0 X1)
     (:RECORD-OFFS (XOR 1) (XOR0)
      ((:RECORD-REF (XOR0 0) (|k-sym11|)
        ((:APP |k-sym11| (XOR0 |sym29| BIT-N 0 X0 X1 0)))))))
    (|:RAND| (RAND |sym32|)
     (:RECORD-REF (RAND 6) (|sym45|)
      ((:RECORD-OFFS (RAND 1) (XOR)
        ((:RECORD-REF (|sym45| 0) (|sym42|)
          ((:<< (|sym42| 13) (|sym41|)
            ((:FIXS
              ((|:sym39| (|sym39| |sym40|)
                (:RECORD-REF (|sym39| 3) (XOR)
                 ((:RECORD-REF (|sym39| 2) (|sym32|)
                   ((:RECORD-REF (|sym39| 1) (|sym45|)
                     ((:POP (4) NIL
                       ((:>> (|sym40| 17) (|sym38|)
                         ((:FIXS
                           ((|:sym36| (|sym36| |sym37|)
                             (:RECORD-REF (|sym36| 3) (XOR)
                              ((:RECORD-REF (|sym36| 2) (|sym32|)
                                ((:RECORD-REF (|sym36| 1) (|sym45|)
                                  ((:POP (4) NIL
                                    ((:<< (|sym37| 15) (|sym35|)
                                      ((:FIXS
                                        ((|:sym33| (|sym33| |sym34|)
                                          (:RECORD-REF (|sym33| 2) (|sym32|)
                                           ((:RECORD-REF (|sym33| 1) (|sym45|)
                                             ((:POP (3) NIL
                                               ((:RECORD-SET!
                                                 (|sym34| 0 |sym45|) NIL
                                                 ((:RECORD-REF (|sym32| 0)
                                                   (|k-sym12|)
                                                   ((:APP |k-sym12|
                                                     (|sym32|
                                                      |sym34|))))))))))))))
                                        (:STACK
                                         ((:LABEL |:sym33|) |sym45| |sym32|)
                                         (|sym33|)
                                         ((:RECORD-REF (XOR 0) (|k-sym13|)
                                           ((:APP |k-sym13|
                                             (XOR |sym33| 32 |sym37|
                                              |sym35|)))))))))))))))))))
    (:STACK ((:LABEL |:sym36|) |sym45| |sym32| XOR)
     (|sym36|)
     ((:RECORD-REF (XOR 0) (|k-sym14|)
       ((:APP |k-sym14|
         (XOR |sym36| 32 |sym40|
          |sym38|)))))))))))))))))))
    (:STACK ((:LABEL |:sym39|) |sym45| |sym32| XOR) (|sym39|)
     ((:RECORD-REF (XOR 0) (|k-sym15|)
       ((:APP |k-sym15|
         (XOR |sym39| 32 |sym42| |sym41|)))))))))))))))))
    (:HEAP
     ((:LABEL |:RAND|) (:LABEL |:XOR|) (:LABEL |:XOR0|) (:LABEL |:GET-BIT0|)
      (:LABEL |:CLEAR-BIT0|) (:LABEL |:XOR-BIT|) |sym45|)
     (RAND)
     ((:FIXS ((|:sym43| (|sym43| |sym44|) (:EXIT (|sym44|) NIL NIL)))
       (:RECORD-REF (RAND 0) (|k-sym16|)
        ((:APP |k-sym16| (RAND |sym43|)))))))))))
