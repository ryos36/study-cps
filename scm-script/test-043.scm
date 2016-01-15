((:fix ((mul3x3-i (j0 i)
          (:if (:= i 3)
               :#t
               (:let ((next-i (:+ i 1)))
                     (mul3x3-j j0 i)
                     (mul3x3-i j0 next-i)))))
  (mul3x3-i 0 0)))
