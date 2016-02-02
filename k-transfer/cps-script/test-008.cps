((:FIXH
  ((XOR0 (|sym12| BIT-N N X0 X1 RV)
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

                          ((:APP XOR0 (|sym12| |sym24| |sym23| |sym22| |sym21| |sym17|))))))))
                     (:APP XOR-BIT (|sym19| |sym28| |sym26|)))))))))))))
          (:APP GET-BIT0 (|sym25| X1)))))
       (:APP GET-BIT0 (|sym27| X0))))))
   (GET-BIT0 (|rsym0| X)
       (:APP |rsym0| (0)))
   (XOR-BIT (|rsym1| A B)
       (:APP |rsym1| (0))))

  (:APP EXIT (0))))
