((:fix ((f0 (a b)
            (:if (:= b 0) a
                 (:+ (:if (:= a b) (f0 (:+ a b) (:- a b))
                          (f0 (:* a b) (:+ a b)))
                     (:* a b)))))
        (:+ (f0 3 5) (f0 4 4))))
