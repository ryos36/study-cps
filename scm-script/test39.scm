((:let ((x 3)) 
       (:fix ((fx (a0)
                  (:+ a0 x))

              (f0 (a0)
                  (:let ((z (fx a0)))
                        (:fix ((f1 (a0) z))
                              f1))))
             (:let ((g0 (f0 3))
                    (g1 (f0 4)))
                   (:+ (g0 0) (g0 1))))))

