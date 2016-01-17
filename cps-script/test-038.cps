((:FIXH
  ((F0 (|sym2| A B)
    (:FIXS ((|sym3| (|sym4|) (:APP |sym2| (|sym4|))))
     (:= (B 0) NIL
      ((:APP |sym3| (A))
       (:FIXS
        ((|sym7| (|sym8|)
          (:* (A B) (|sym6|)
           ((:+ (|sym8| |sym6|) (|sym5|) ((:APP |sym3| (|sym5|))))))))
        (:= (A B) NIL
         ((:+ (A B) (|sym12|)
           ((:- (A B) (|sym11|)
             ((:FIXS ((|sym9| (|sym10|) (:APP |sym7| (|sym10|))))
               (:APP F0 (|sym9| |sym12| |sym11|)))))))
          (:* (A B) (|sym16|)
           ((:+ (A B) (|sym15|)
             ((:FIXS ((|sym13| (|sym14|) (:APP |sym7| (|sym14|))))
               (:APP F0 (|sym13| |sym16| |sym15|)))))))))))))))
  (:FIXS
   ((|sym20| (|sym21|)
     (:FIXS
      ((|sym18| (|sym19|)
        (:+ (|sym21| |sym19|) (|sym17|) ((:APP EXIT (|sym17|))))))
      (:APP F0 (|sym18| 4 4)))))
   (:APP F0 (|sym20| 3 5)))))
