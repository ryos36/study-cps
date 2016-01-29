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
(:APP EXIT (:UNSPECIFIED)))

    (:FIXH
     ((|:MUL3X3| (MUL3X3 |sym2| A B C)
       (:RECORD-REF (MUL3X3 2) (MUL3X3-JI)
        ((:RECORD-REF (MUL3X3 1) (MUL3X3-J)
          ((:FIXH
            ((|:MUL3X3-JI| (MUL3X3-JI |sym3| J0 I0)
              (:RECORD-REF (MUL3X3-JI 1) (|k-sym1|)
               ((:RECORD-REF (|k-sym1| 3) (A)
                 ((:RECORD-REF (|k-sym1| 2) (B)
                   ((:RECORD-REF (|k-sym1| 1) (C)
                     ((:* (I0 3) (|sym8|)
                       ((:RECORD-REF (A |sym8|) (|sym7|)
                         ((:RECORD-REF (B J0) (|sym6|)
                           ((:* (|sym7| |sym6|) (|sym5|)
                             ((:+ (|sym8| J0) (|sym4|)
                               ((:RECORD-SET! (|sym5| |sym4| C) NIL
                                 ((:RECORD-REF (|sym3| 0) (|k-sym2|)
                                   ((:APP |k-sym2|
                                     (|sym3| :UNSPECIFIED)))))))))))))))))))))))))
             (|:MUL3X3-J| (MUL3X3-J |sym9| J I0)
              (:RECORD-REF (MUL3X3-J 1) (|k-sym3|)
               ((:RECORD-REF (|k-sym3| 4) (|k-sym9|)
                 ((:RECORD-REF (|k-sym9| 2) (MUL3X3-JI)
                   ((:FIXH
                     ((|:MUL3X3-J-I0| (MUL3X3-J-I0 |sym10| J)
                       (:RECORD-REF (MUL3X3-J-I0 2) (|k-sym7|)
                        ((:RECORD-REF (|k-sym7| 2) (MUL3X3-JI)
                          ((:RECORD-REF (MUL3X3-J-I0 1) (I0)
                            ((:= (J 3) NIL
                              ((:RECORD-REF (|sym10| 0) (|k-sym4|)
                                ((:APP |k-sym4| (|sym10| :|#T|))))
                               (:+ (J 1) (|sym17|)
                                ((:FIXS
                                  ((|:sym15| (|sym15| |sym16|)
                                    (:RECORD-REF (|sym15| 3) (MUL3X3-J-I0)
                                     ((:RECORD-REF (|sym15| 2) (|sym10|)
                                       ((:RECORD-REF (|sym15| 1) (|sym17|)
                                         ((:POP (4) NIL
                                           ((:RECORD-REF (MUL3X3-J-I0 0) (|k-sym5|)
                                             ((:APP |k-sym5|
                                               (MUL3X3-J-I0 |sym10|
                                                |sym17|))))))))))))))
                                  (:STACK
                                   ((:LABEL |:sym15|) |sym17| |sym10| MUL3X3-J-I0)
                                   (|sym15|)
                                   ((:RECORD-REF (MUL3X3-JI 0) (|k-sym6|)
                                     ((:APP |k-sym6|
                                       (MUL3X3-JI |sym15| J I0)))))))))))))))))))
    (:HEAP ((:LABEL |:MUL3X3-J-I0|) I0 MUL3X3) (MUL3X3-J-I0)
     ((:RECORD-REF (MUL3X3-J-I0 0) (|k-sym8|)
       ((:APP |k-sym8| (MUL3X3-J-I0 |sym9| J))))))))))))))
    (|:MUL3X3-I| (MUL3X3-I |sym20| I)
     (:RECORD-REF (MUL3X3-I 1) (|k-sym10|)
      ((:RECORD-REF (|k-sym10| 4) (|k-sym14|)
        ((:RECORD-REF (|k-sym14| 1) (MUL3X3-J)
          ((:= (I 3) NIL
            ((:RECORD-REF (|sym20| 0) (|k-sym11|)
              ((:APP |k-sym11| (|sym20| :|#T|))))
             (:+ (I 1) (|sym27|)
              ((:FIXS
                ((|:sym25| (|sym25| |sym26|)
                  (:RECORD-REF (|sym25| 3) (MUL3X3-I)
                   ((:RECORD-REF (|sym25| 2) (|sym20|)
                     ((:RECORD-REF (|sym25| 1) (|sym27|)
                       ((:POP (4) NIL
                         ((:RECORD-REF (MUL3X3-I 0) (|k-sym12|)
                           ((:APP |k-sym12|
                             (MUL3X3-I |sym20| |sym27|))))))))))))))
                (:STACK ((:LABEL |:sym25|) |sym27| |sym20| MUL3X3-I)
                 (|sym25|)
                 ((:RECORD-REF (MUL3X3-J 0) (|k-sym13|)
                   ((:APP |k-sym13| (MUL3X3-J |sym25| I)))))))))))))))))))
    (:HEAP ((:LABEL :DUMMY) C B A MUL3X3) (|k-sym0|)
     ((:HEAP ((:LABEL |:MUL3X3-JI|) |k-sym0|) (MUL3X3-JI)
       ((:HEAP ((:LABEL |:MUL3X3-J|) |k-sym0|) (MUL3X3-J)
         ((:HEAP ((:LABEL |:MUL3X3-I|) |k-sym0|) (MUL3X3-I)
           ((:RECORD-REF (MUL3X3-I 0) (|k-sym15|)
             ((:APP |k-sym15| (MUL3X3-I |sym2| 0 0)))))))))))))))))))
(:HEAP ((:LABEL |:MUL3X3|) MUL3X3-J MUL3X3-JI) (MUL3X3)
 ((:RECORD-REF (EXIT 0) (|k-sym16|) ((:APP |k-sym16| (EXIT :UNSPECIFIED))))))))
