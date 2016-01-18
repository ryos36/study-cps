((:fix ((f0 (a0 a1 a2) 
            (gunc ex2 a0)
            (:let ((l0 (:+ (:- (:+ a0 ex0) (:* a1 ex1))))
                   (l1 (:+ ex0 ex1))
                   (ex2 (:+ 7 8)))
              (func l0 l1)
              (:fix ((g0 (b0 b1 b2)
                         (func 
                           (:+ l0 l1)
                           (:+ ex0 ex-ex0))))
                    (g0 ex2 2 3)))))
       (f0 4 5 6)))

