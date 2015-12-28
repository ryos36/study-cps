((:let ((r (:heap 0 1 2 3)))
   (:record-set! r 0 10)
   (:+ (:record-ref r 0) (:record-ref r 1))))
