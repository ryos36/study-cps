((:FIXH
  ((EVEN? (|sym2| X)
    (:= (X 0) NIL
     ((:APP |sym2| (:|#T|)) (:- (X 1) (|sym7|) ((:APP ODD? (|sym2| |sym7|)))))))
   (ODD? (|sym8| X)
    (:= (X 0) NIL
     ((:APP |sym8| (:|#T|))
      (:- (X 1) (|sym13|) ((:APP EVEN? (|sym8| |sym13|))))))))
  (:APP EVEN? (EXIT 997)))

 (:FIXH
  ((|:EVEN?| (EVEN? |sym2| X)
    (:RECORD-REF (EVEN? 1) (|k-sym1|)
     ((:RECORD-REF (|k-sym1| 2) (ODD?)
       ((:= (X 0) NIL
         ((:RECORD-REF (|sym2| 0) (|k-sym2|) ((:APP |k-sym2| (|sym2| :|#T|))))
          (:- (X 1) (|sym7|)
           ((:RECORD-REF (ODD? 0) (|k-sym3|)
             ((:APP |k-sym3| (ODD? |sym2| |sym7|)))))))))))))
   (|:ODD?| (ODD? |sym8| X)
    (:RECORD-REF (ODD? 1) (|k-sym4|)
     ((:RECORD-REF (|k-sym4| 1) (EVEN?)
       ((:= (X 0) NIL
         ((:RECORD-REF (|sym8| 0) (|k-sym5|) ((:APP |k-sym5| (|sym8| :|#T|))))
          (:- (X 1) (|sym13|)
           ((:RECORD-REF (EVEN? 0) (|k-sym6|)
             ((:APP |k-sym6| (EVEN? |sym8| |sym13|))))))))))))))
  (:HEAP ((:LABEL :DUMMY) EVEN? ODD?) (|k-sym0|)
   ((:HEAP ((:LABEL |:EVEN?|) |k-sym0|) (EVEN?)
     ((:HEAP ((:LABEL |:ODD?|) |k-sym0|) (ODD?)
       ((:RECORD-REF (EVEN? 0) (|k-sym7|)
         ((:APP |k-sym7| (EVEN? EXIT 997))))))))))))
