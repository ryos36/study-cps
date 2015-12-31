((:FIX
 ((FIBO-2 (sym2 N A-1 A-2)
   (:FIX ((sym3 (sym4) (:APP sym2 (sym4))))
    (:= (N 0) NIL
     ((:APP sym3 (A-1))
      (:- (N 1) (sym8)
       ((:+ (A-1 A-2) (sym7)
         ((:FIX ((sym5 (sym6) (:APP sym3 (sym6))))
           (:APP FIBO-2 (sym5 sym8 sym7 A-1)))))))))))
  (FIBO (sym9 N)
   (:FIX ((sym10 (sym11) (:APP sym9 (sym11)))) (:APP FIBO-2 (sym10 N 1 0)))))
 (:FIX ((sym12 (sym13) (:EXIT (sym13) NIL NIL))) (:APP FIBO (sym12 8)))))
