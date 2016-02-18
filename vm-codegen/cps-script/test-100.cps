((:FIXH
  ((|:MUL3X3| (MUL3X3 |sym1| A B C)
    (:RECORD-REF (MUL3X3 2) (MUL3X3-J)
     ((:RECORD-REF (MUL3X3 1) (MUL3X3-JI)
       ((:FIXH
         ((|:MUL3X3-JI| (MUL3X3-JI |sym2| J0 I0)
           (:RECORD-REF (MUL3X3-JI 3) (A)
            ((:RECORD-REF (MUL3X3-JI 2) (B)
              ((:RECORD-REF (MUL3X3-JI 1) (C)
                ((:* (I0 3) (|sym7|)
                  ((:RECORD-REF (A |sym7|) (|sym6|)
                    ((:RECORD-REF (B J0) (|sym5|)
                      ((:* (|sym6| |sym5|) (|sym4|)
                        ((:+ (|sym7| J0) (|sym3|)
                          ((:RECORD-SET! (|sym4| |sym3| C) NIL
                            ((:RECORD-REF (|sym2| 0) (|k-sym0|)
                              ((:APP |k-sym0|
                                (|sym2| :UNSPECIFIED)))))))))))))))))))))))
          (|:MUL3X3-J| (MUL3X3-J |sym8| J I0)
           (:RECORD-OFFS (MUL3X3-J 1) (MUL3X3-JI)
            ((:= (J 3) NIL
              ((:RECORD-REF (|sym8| 0) (|k-sym1|)
                ((:APP |k-sym1| (|sym8| :|#T|))))
               (:+ (J 1) (|sym15|)
                ((:FIXS
                  ((|:sym13| (|sym13| |sym14|)
                    (:RECORD-REF (|sym13| 4) (I0)
                     ((:RECORD-REF (|sym13| 3) (|sym15|)
                       ((:RECORD-REF (|sym13| 2) (|sym8|)
                         ((:RECORD-REF (|sym13| 1) (MUL3X3-J)
                           ((:POP (5) NIL
                             ((:RECORD-REF (MUL3X3-J 0) (|k-sym2|)
                               ((:APP |k-sym2|
                                 (MUL3X3-J |sym8| |sym15| I0))))))))))))))))
                  (:STACK ((:LABEL |:sym13|) MUL3X3-J |sym8| |sym15| I0)
                   (|sym13|)
                   ((:RECORD-REF (MUL3X3-JI 0) (|k-sym3|)
                     ((:APP |k-sym3| (MUL3X3-JI |sym13| J I0))))))))))))))
          (|:MUL3X3-I| (MUL3X3-I |sym16| J0 I)
           (:RECORD-OFFS (MUL3X3-I 1) (MUL3X3-J)
            ((:= (I 3) NIL
              ((:RECORD-REF (|sym16| 0) (|k-sym4|)
                ((:APP |k-sym4| (|sym16| :|#T|))))
               (:+ (I 1) (|sym23|)
                ((:FIXS
                  ((|:sym21| (|sym21| |sym22|)
                    (:RECORD-REF (|sym21| 4) (|sym23|)
                     ((:RECORD-REF (|sym21| 3) (J0)
                       ((:RECORD-REF (|sym21| 2) (|sym16|)
                         ((:RECORD-REF (|sym21| 1) (MUL3X3-I)
                           ((:POP (5) NIL
                             ((:RECORD-REF (MUL3X3-I 0) (|k-sym5|)
                               ((:APP |k-sym5|
                                 (MUL3X3-I |sym16| J0 |sym23|))))))))))))))))
                  (:STACK ((:LABEL |:sym21|) MUL3X3-I |sym16| J0 |sym23|)
                   (|sym21|)
                   ((:RECORD-REF (MUL3X3-J 0) (|k-sym6|)
                     ((:APP |k-sym6| (MUL3X3-J |sym21| J0 I)))))))))))))))
         (:HEAP
          ((:LABEL |:MUL3X3-I|) (:LABEL |:MUL3X3-J|) (:LABEL |:MUL3X3-JI|) C B
           A MUL3X3)
          (MUL3X3-I)
          ((:RECORD-REF (MUL3X3-I 0) (|k-sym7|)
            ((:APP |k-sym7| (MUL3X3-I |sym1| 0 0)))))))))))))
  (:HEAP ((:LABEL |:MUL3X3|) MUL3X3-JI MUL3X3-J) (|k-sym9|)
   ((:RECORD-REF (EXIT 0) (|k-sym8|) ((:APP |k-sym8| (EXIT :UNSPECIFIED))))))))
