((:FIXH
  ((EVEN? (|sym2| X)
    (:= (X 0) NIL
     ((:APP |sym2| (:|#T|)) (:- (X 1) (|sym7|) ((:APP ODD? (|sym2| |sym7|)))))))
   (ODD? (|sym8| X)
    (:= (X 0) NIL
     ((:APP |sym8| (:|#T|))
      (:- (X 1) (|sym13|) ((:APP EVEN? (|sym8| |sym13|))))))))
  (:APP EVEN? (EXIT 997))))
