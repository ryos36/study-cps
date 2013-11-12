((:define x (:heap 1 2 3))
 x
 (:record-ref x 2)
 (:record-set! x 1 (:heap 4 5 6))
 (:record-ref
    (:record-ref x 1 ) 2))
 
