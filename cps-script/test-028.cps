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
          (:FIXS ((|sym11| (|sym12|) (:APP |sym10| (|sym12|))))
           (:= (J 3) NIL
            ((:APP |sym11| (:|#T|))
             (:+ (J 1) (|sym17|)
              ((:FIXS
                ((|sym15| (|sym16|)
                  (:FIXS ((|sym13| (|sym14|) (:APP |sym11| (|sym14|))))
                   (:APP MUL3X3-J-I0 (|sym13| |sym17|)))))
                (:APP MUL3X3-JI (|sym15| J I0))))))))))
        (:FIXS ((|sym18| (|sym19|) (:APP |sym9| (|sym19|))))
         (:APP MUL3X3-J-I0 (|sym18| J)))))
      (MUL3X3-I (|sym20| I)
       (:FIXS ((|sym21| (|sym22|) (:APP |sym20| (|sym22|))))
        (:= (I 3) NIL
         ((:APP |sym21| (:|#T|))
          (:+ (I 1) (|sym27|)
           ((:FIXS
             ((|sym25| (|sym26|)
               (:FIXS ((|sym23| (|sym24|) (:APP |sym21| (|sym24|))))
                (:APP MUL3X3-I (|sym23| |sym27|)))))
             (:APP MUL3X3-J (|sym25| I))))))))))
     (:FIXS ((|sym28| (|sym29|) (:APP |sym2| (|sym29|))))
      (:APP MUL3X3-I (|sym28| 0 0))))))
  (:APP EXIT (:UNSPECIFIED))))
