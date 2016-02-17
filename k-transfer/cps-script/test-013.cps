((:FIXH
  ((FIBO-2 (|sym2| N A-1 A-2)
    (:= (N 0) NIL
     ((:APP |sym2| (A-1))
      (:- (N 1) (|sym8|)
       ((:+ (A-1 A-2) (|sym7|) ((:APP FIBO-2 (|sym2| |sym8| |sym7| A-1)))))))))
   (FIBO (|sym9| N) (:APP FIBO-2 (|sym9| N 1 0))))
  (:APP FIBO (EXIT 8))))
