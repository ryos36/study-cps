((:FIXS
 ((|sym17| (|sym18|)
   (:FIXS
    ((|sym15| (|sym16|)
      (:- (BIT-N 1) (|sym14|)
       ((:+ (N 1) (|sym13|)
         ((:>> (X0 1) (|sym12|)
           ((:>> (X1 1) (|sym11|)
             ((:FIXS
               ((|sym9| (|sym10|)
                 (:<< (|sym10| N) (|sym8|)
                  ((:+ (RV |sym8|) (|sym7|)
                    ((:FIXS ((|sym5| (|sym6|) (:APP |sym3| (|sym6|))))
                      (:APP XOR0
                       (|sym5| |sym14| |sym13| |sym12| |sym11|
                        |sym7|)))))))))
               (:APP XOR-BIT (|sym9| |sym18| |sym16|)))))))))))))
    (:APP GET-BIT0 (|sym15| X1)))))
 (:APP GET-BIT0 (|sym17| X0))))

