((:FIXH
  ((MUL3X3-I (|sym2| J0 I)
   (:FIXS ((|sym3| (|sym4|) (:APP |sym2| (|sym4|))))
    (:= (I 3) NIL
     ((:APP |sym3| (:|#T|))
      (:+ (I 1) (|sym9|)
       ((:FIXS
         ((|sym7| (|sym8|)
           (:FIXS ((|sym5| (|sym6|) (:APP |sym3| (|sym6|))))
            (:APP MUL3X3-I (|sym5| J0 |sym9|)))))
         (:APP MUL3X3-J (|sym7| J0 I))))))))))
 (:APP EXIT (:UNSPECIFIED))))
