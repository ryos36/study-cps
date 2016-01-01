((:FIXH
  ((FX (sym2 A0) (:+ (A0 3) (sym3) ((:APP sym2 (sym3)))))
   (F0 (sym4 A0)
    (:FIXS ((sym6 (sym7) (:FIXH ((F1 (sym5 A0) (:APP sym5 (sym7)))) (:APP sym4 (F1)))))
     (:APP FX (sym6 A0)))))
  (:FIXS
   ((sym15 (sym16)
     (:FIXS
      ((sym13 (sym14)
        (:FIXS
         ((sym11 (sym12)
           (:FIXS
            ((sym9 (sym10) (:+ (sym12 sym10) (sym8) ((:EXIT (sym8) NIL NIL)))))
            (:APP sym14 (sym9 1)))))
         (:APP sym16 (sym11 0)))))
      (:APP F0 (sym13 4)))))
   (:APP F0 (sym15 3)))))
