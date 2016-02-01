((:FIXH
  ((EVEN? (|sym2| X)
    (:= (X 0) NIL
        ((:APP ODD? (|sym2| X))
         (:APP OTHERS? (|sym2| X)))))

   (ODD? (|sym8| X)
    (:= (X 0) NIL
        ((:APP OTHERS? (|sym8| X))
         (:APP EVEN? (|sym8| X)))))

   (OTHERS? (|sym7| X)
    (:= (X 0) NIL
        ((:APP EVEN? (|sym7| X))
         (:APP ODD? (|sym7| X))))))

  (:APP EVEN? (EXIT 997))))
