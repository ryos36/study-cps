(:define x (:heap 0 1 2 3))
(:fix ((f (b a x) (:let ((aa x)) 
                   (:record-set! x b a) 
                   (:let ((b0 (:record-ref x 0))
                          (b1 (:record-ref x 1))
                          (b2 (:record-ref x 1))
                          (b3 (:record-ref x 1)))
                   (exit (:+ 10000
                             (:+ 
                              (:+
                               (:* b0 1000)
                               (:* b1 100))
                              (:+
                               (:* b2 10)
                               (:* b3 1)))))))))
      (f 2 5 x))
