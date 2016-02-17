((:FIXH
  ((FIBO (|sym1| N)
    (:= (N 0) NIL
     ((:APP |sym1| (1))
      (:= (N 1) NIL
       ((:APP |sym1| (1))
        (:- (N 1) (|sym12|)
         ((:FIXH
           ((|sym10| (|sym11|)
             (:- (N 2) (|sym9|)
              ((:FIXH
                ((|sym7| (|sym8|)
                  (:+ (|sym11| |sym8|) (|sym6|) ((:APP |sym1| (|sym6|))))))
                (:APP FIBO (|sym7| |sym9|)))))))
           (:APP FIBO (|sym10| |sym12|)))))))))))
  (:APP FIBO (EXIT 8))))
