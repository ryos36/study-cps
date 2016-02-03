((:FIXH
(|:XOR0| (XOR0 |sym12| BIT-N N X0 X1 RV)
  (:RECORD-OFFS (XOR0 3) (XOR-BIT)
   ((:RECORD-OFFS (XOR0 1) (GET-BIT0)
     ((:= (0 BIT-N) NIL
       ((:RECORD-REF (|sym12| 0) (|k-sym6|) ((:APP |k-sym6| (|sym12| RV))))
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
                                        ((:RECORD-REF (|sym19| 5) (|sym12|)
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
                                                          (|k-sym7|)
                                                          ((:APP |k-sym7|
                                                            (XOR0 |sym12|
                                                             |sym24| |sym23|
                                                             |sym22| |sym21|
                                                             |sym17|))))))))))))))))))))))))))))
                                                             (:STACK
                                                              ((:LABEL |:sym19|) |sym21| |sym22| |sym23|
                                                               |sym24| |sym12| XOR0 RV N)
                                                              (|sym19|)
                                                              ((:RECORD-REF (XOR-BIT 0) (|k-sym8|)
                                                                ((:APP |k-sym8|
                                                                  (XOR-BIT |sym19| |sym28|
                                                                   |sym26|)))))))))))))))))))))))))))
    (:STACK ((:LABEL |:sym25|) X1 X0 N BIT-N) (|sym25|)
     ((:RECORD-REF (GET-BIT0 0) (|k-sym9|)
       ((:APP |k-sym9| (GET-BIT0 |sym25| X1)))))))))
(:RECORD-REF (GET-BIT0 0) (|k-sym10|)
 ((:APP |k-sym10| (GET-BIT0 |sym27| X0))))))))))))

(:RECORD-REF (XOR 0) (|k-sym15|)
 ((:APP |k-sym15| (XOR EXIT 32 4 5))))))
