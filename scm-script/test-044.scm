((:define (mul3x3-i j0 dummy i)
   (:if (:= i 3)
        :#t
        (:let ((next-i (:+ i 1)))
          (mul3x3-j j0 (:+ 3 5) i)
          (mul3x3-i j0 4 next-i)))))
