((:FIXH
  ((MUL3X3 (|sym2| A B C)
    (:FIXH
     ((MUL3X3-JI (|sym3| J0 I0)
       (:* (I0 3) (|sym10|)
        ((:FIXS
          ((|sym8| (|sym9|)
            (:FIXS
             ((|sym6| (|sym7|)
               (:+ (|sym10| J0) (|sym5|)
                ((:* (|sym9| |sym7|) (|sym4|)
                  ((:RECORD-SET! (C |sym5| |sym4|) NIL
                    (:APP |sym3| (:UNSPECIFIED)))))))))
             (:APP :RECORED-REF (|sym6| B J0)))))
          (:APP :RECORED-REF (|sym8| A |sym10|))))))
      (MUL3X3-J (|sym11| J I0)
       (:FIXH
        ((MUL3X3-J-I0 (|sym12| J)
          (:FIXS ((|sym13| (|sym14|) (:APP |sym12| (|sym14|))))
           (:= (J 3) NIL
            ((:APP |sym13| (:|#T|))
             (:+ (J 1) (|sym19|)
              ((:FIXS
                ((|sym17| (|sym18|)
                  (:FIXS ((|sym15| (|sym16|) (:APP |sym13| (|sym16|))))
                   (:APP MUL3X3-J-I0 (|sym15| |sym19|)))))
                (:APP MUL3X3-JI (|sym17| J I0))))))))))
        (:FIXS ((|sym20| (|sym21|) (:APP |sym11| (|sym21|))))
         (:APP MUL3X3-J-I0 (|sym20| J)))))
      (MUL3X3-I (|sym22| I)
       (:FIXS ((|sym23| (|sym24|) (:APP |sym22| (|sym24|))))
        (:= (I 3) NIL
         ((:APP |sym23| (:|#T|))
          (:+ (I 1) (|sym29|)
           ((:FIXS
             ((|sym27| (|sym28|)
               (:FIXS ((|sym25| (|sym26|) (:APP |sym23| (|sym26|))))
                (:APP MUL3X3-I (|sym25| |sym29|)))))
             (:APP MUL3X3-J (|sym27| I))))))))))
     (:FIXS ((|sym30| (|sym31|) (:APP |sym2| (|sym31|))))
      (:APP MUL3X3-I (|sym30| 0 0))))))
  (:APP EXIT (:UNSPECIFIED))))
