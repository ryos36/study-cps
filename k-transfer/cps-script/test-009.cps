((:FIXH
  ((XOR0 (|sym12| BIT-N)
    (:= (0 BIT-N) NIL
     ((:APP |sym12| (RV))
      (:FIXS
       ((|sym27| (|sym28|)
         (:FIXS
          ((|sym25| (|sym26|)
            (:APP XOR0 (|sym12| |sym26|))))
          (:APP GET-BIT0 (|sym25| X1)))))
       (:APP GET-BIT0 (|sym27| X0))))))
   (GET-BIT0 (|rsym0| X)
       (:APP |rsym0| (0)))
   (XOR-BIT (|rsym1| A B)
       (:APP |rsym1| (0))))

  (:APP EXIT (0))))
