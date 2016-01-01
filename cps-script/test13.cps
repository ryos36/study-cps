((:FIX
 ((ETA-IF (sym2 X)
   (:FIX ((sym3 (sym4) (:APP sym2 (sym4))))
    (:= (X 0) NIL
     ((:FIX ((sym5 (sym6) (:APP sym3 (sym6)))) (:APP F (sym5 X)))
      (:FIX ((sym7 (sym8) (:APP sym3 (sym8)))) (:APP G (sym7 X))))))))
 (:EXIT (UNSPECIFIED) NIL NIL)))
