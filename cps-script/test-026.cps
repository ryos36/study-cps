((:FIXH
  ((MUL3X3 (|sym2| A B C)
    (:FIXH
     ((MUL3X3-JI (|sym3| J0 I0)
       (:* (I0 3) (|sym10|)
        ((:FIXS
          ((|sym8| (|sym9|)
            (:FIXS
             ((|sym6| (|sym7|)
               (:* (|sym9| |sym7|) (|sym5|)
                ((:+ (|sym10| J0) (|sym4|)
                  ((:RECORD-SET! (|sym5| |sym4| C) NIL
                    (:APP |sym3| (:UNSPECIFIED)))))))))
             (:APP :RECORED-REF (|sym6| B J0)))))
          (:APP :RECORED-REF (|sym8| A |sym10|))))))
      (MUL3X3-J (|sym11| J I0)
       (:FIXS ((|sym12| (|sym13|) (:APP |sym11| (|sym13|))))
        (:= (J 3) NIL
         ((:APP |sym12| (:|#T|))
          (:+ (J 1) (|sym18|)
           ((:FIXS
             ((|sym16| (|sym17|)
               (:FIXS ((|sym14| (|sym15|) (:APP |sym12| (|sym15|))))
                (:APP MUL3X3-J (|sym14| |sym18| I0)))))
             (:APP MUL3X3-JI (|sym16| J I0)))))))))
      (MUL3X3-I (|sym19| J0 I)
       (:FIXS ((|sym20| (|sym21|) (:APP |sym19| (|sym21|))))
        (:= (I 3) NIL
         ((:APP |sym20| (:|#T|))
          (:+ (I 1) (|sym26|)
           ((:FIXS
             ((|sym24| (|sym25|)
               (:FIXS ((|sym22| (|sym23|) (:APP |sym20| (|sym23|))))
                (:APP MUL3X3-I (|sym22| J0 |sym26|)))))
             (:APP MUL3X3-J (|sym24| J0 I))))))))))
     (:FIXS ((|sym27| (|sym28|) (:APP |sym2| (|sym28|))))
      (:APP MUL3X3-I (|sym27| 0 0))))))
  (:APP EXIT (:UNSPECIFIED))))
