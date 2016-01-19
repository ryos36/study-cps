((:FIXH
  ((F0 (|sym2| A0 A1 A2)
    (:+ (A0 EX0) (|sym29|)
     ((:* (A1 EX1) (|sym28|)
       ((:- (|sym29| |sym28|) (|sym27|)
         ((:+ (|sym27| NIL) (|sym26|)
           ((:+ (EX0 EX1) (|sym25|)
             ((:+ (7 EX-F0) (|sym24|)
               ((:FIXS
                 ((|sym22| (|sym23|)
                   (:FIXH
                    ((G0 (|sym3| B0 B1 B2)
                      (:+ (EX-G0 EX0) (|sym17|)
                       ((:+ (|sym24| EX3) (|sym16|)
                         ((:FIXH
                           ((H0 (|sym4| C0 C1 C2)
                             (:+ (EX-H0 |sym17|) (|sym11|)
                              ((:+ (|sym26| |sym25|) (|sym10|)
                                ((:+ (|sym16| EX4) (|sym9|)
                                  ((:+ (EX1 EX3) (|sym8|)
                                    ((:+ (|sym24| EX4) (|sym7|)
                                      ((:APP FUNC
                                        (|sym4| |sym8| |sym10|))))))))))))))
                           (:FIXS
                            ((|sym14| (|sym15|)
                              (:APP H0 (|sym3| |sym25| G0 |sym15| |sym17|))))
                            (:APP 1 (|sym14| 2 3))))))))))
                    (:FIXS
                     ((|sym20| (|sym21|)
                       (:APP G0 (|sym2| |sym24| |sym21| |sym26|))))
                     (:APP F0 (|sym20| 2 3))))))
(:APP FUNC (|sym22| |sym26| |sym25|)))))))))))))))))
(:APP F0 (EXIT 4 5 6))))
