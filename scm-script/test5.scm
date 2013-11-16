((:define f
  (:let ((v0 129))
   (:define (f0 a) (:if a
                    (:let ((temp 0))
                     (:define (g x) (:+ v0 11))
                     g)
                    (:+ v0 100)))
   f0
  ))

 (f :#f)
 (:define g (f :#t))
 (:let ((v0 1103))
  (g 55)))
