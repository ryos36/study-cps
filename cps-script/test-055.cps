((:FIXH
  ((F0 (|sym2| A0 A1 A2)
    (:+ (EX0 EX1) (|sym14|)
     ((:FIXH
       ((G0 (|sym4| EX0 B1 B2)
         (:+ (EX0 EX2) (|sym11|)
          ((:FIXH
            ((H0 (|sym6| C0 C1 C2)
              (:+ (EX0 EX1) (|sym8|)
               ((:* (|sym8| EX2) (|sym7|) ((:APP |sym6| (|sym7|))))))))
            (:FIXS
             ((|sym9| (|sym10|)
               (:* (|sym11| |sym10|) (|sym5|) ((:APP |sym4| (|sym5|))))))
             (:APP H0 (|sym9| 1 2 3))))))))
       (:FIXS
        ((|sym12| (|sym13|)
          (:* (|sym14| |sym13|) (|sym3|) ((:APP |sym2| (|sym3|))))))
        (:APP G0 (|sym12| 4 5 6))))))))
  (:APP F0 (EXIT 7 8 9))))
