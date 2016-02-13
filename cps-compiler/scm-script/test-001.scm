(:fix ((even? (x) (:if (:= x 0) :#t (odd? (:- x 1))))
        (odd? (x) (:if (:= x 0) :#f (even? (:- x 1)))))
   (even? 10))
