((:FIXH
 ((MUL3X3 (|sym2| A B C)
   (:FIXH
    ((MUL3X3-JI (|sym3| J0 I0)
      (:* (I0 3) (|sym12|)
       ((:FIXS
         ((|sym10| (|sym11|)
           (:FIXS
            ((|sym8| (|sym9|)
              (:+ (|sym12| J0) (|sym7|)
               ((:* (|sym11| |sym9|) (|sym6|)
                 ((:FIXS ((|sym4| (|sym5|) (:APP |sym3| (|sym5|))))
                   (:APP RECORD-SET! (|sym4| C |sym7| |sym6|)))))))))
            (:APP RECORED-REF (|sym8| B J0)))))
         (:APP RECORED-REF (|sym10| A |sym12|))))))
     (MUL3X3-J (|sym13| J I0)
      (:FIXS ((|sym14| (|sym15|) (:APP |sym13| (|sym15|))))
       (:= (J 3) NIL
        ((:APP |sym14| (:|#T|))
         (:+ (J 1) (|sym20|)
          ((:FIXS
            ((|sym18| (|sym19|)
              (:FIXS ((|sym16| (|sym17|) (:APP |sym14| (|sym17|))))
               (:APP MUL3X3-J (|sym16| |sym20| I0)))))
            (:APP MUL3X3-JI (|sym18| J I0)))))))))
     (MUL3X3-I (|sym21| J0 I)
      (:FIXS ((|sym22| (|sym23|) (:APP |sym21| (|sym23|))))
       (:= (I 3) NIL
        ((:APP |sym22| (:|#T|))
         (:+ (I 1) (|sym28|)
          ((:FIXS
            ((|sym26| (|sym27|)
              (:FIXS ((|sym24| (|sym25|) (:APP |sym22| (|sym25|))))
               (:APP MUL3X3-I (|sym24| J0 |sym28|)))))
            (:APP MUL3X3-J (|sym26| J0 I))))))))))
    (:FIXS ((|sym29| (|sym30|) (:APP |sym2| (|sym30|))))
     (:APP MUL3X3-I (|sym29| 0 0))))))
 (:APP EXIT (:UNSPECIFIED))))
