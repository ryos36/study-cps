((:define y (:heap 9 10 11))
 (:define x (:heap y 2 3))
 x
 (:record-ref x 2)
 (:record-set! x 1 (:heap 4 5 6))
 (:record-ref
    (:record-ref x 1 ) 2)
 (:record-ref
    (:record-ref x 0) 2))
 
