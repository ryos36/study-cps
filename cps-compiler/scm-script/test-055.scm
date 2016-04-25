(:define x 43)
(:define y 40)
(:fix ((g (a b c) (:+ (:* (:+ x a) b) (:* c y))))
      (g 1 2 3))
