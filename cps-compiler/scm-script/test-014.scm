(:define gv0 32)
(:define gv 32)
(:fix ((f (a) (:* (:+ a gv)
                  (:fix ((g (b) (:* gv (:+ b gv))))
                        (g 4)))))
      (:exit 
        (f 35)))
(:fix ((f3 (a) (:* 3 a)))
      (f3 2))
