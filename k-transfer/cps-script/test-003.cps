((:FIXH
  ((MUL3X3 (|sym2| A B C)
    (:FIXH
     ((MUL3X3-JI (|sym3| J0 I0)
       (:* (I0 3) (|sym8|)
        ((:RECORD-REF (A |sym8|) (|sym7|)
          ((:RECORD-REF (B J0) (|sym6|)
            ((:* (|sym7| |sym6|) (|sym5|)
              ((:+ (|sym8| J0) (|sym4|)
                ((:RECORD-SET! (|sym5| |sym4| C) NIL
                  ((:APP |sym3| (:UNSPECIFIED)))))))))))))))
      (MUL3X3-J (|sym9| J I0)
       (:FIXH
        ((MUL3X3-J-I0 (|sym10| J)
          (:= (J 3) NIL
           ((:APP |sym10| (:|#T|))
            (:+ (J 1) (|sym17|)
             ((:FIXS ((|sym15| (|sym16|) (:APP MUL3X3-J-I0 (|sym10| |sym17|))))
               (:APP MUL3X3-JI (|sym15| J I0)))))))))
        (:APP MUL3X3-J-I0 (|sym9| J))))
      (MUL3X3-I (|sym20| I)
       (:= (I 3) NIL
        ((:APP |sym20| (:|#T|))
         (:+ (I 1) (|sym27|)
          ((:FIXS ((|sym25| (|sym26|) (:APP MUL3X3-I (|sym20| |sym27|))))
            (:APP MUL3X3-J (|sym25| I)))))))))
(:APP MUL3X3-I (|sym2| 0 0)))))
(:APP EXIT (:UNSPECIFIED))))
