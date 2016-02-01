((:FIXH
  ((EVEN? (|sym2| X)
      (:APP ODD? (|sym2| X)))

   (ODD? (|sym8| X)
      (:APP OTHERS? (|sym8| X)))

   (OTHERS? (|sym7| X)
      (:APP EVEN? (|sym7| X))))

  (:APP EVEN? (EXIT 997))))
