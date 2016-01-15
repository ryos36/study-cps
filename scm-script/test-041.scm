((:define (mul3x3 a b c)
   (:fix ((mul3x3-ji (j0 i0)
           (:let ((j-start (:* i0 3))
                  (i-start j0))
             (:let ((a-ji (:record-ref a j-start))
                    (b-ji (:record-ref b i-start)))
                   (:record-set! (:* a-ji b-ji) (:+ j-start i-start) c))))

          (mul3x3-j (j i0)
            (:if (:= j 3) 
               :#t
               (:let ((next-j (:+ j 1)))
                 (mul3x3-ji j i0)
                 (mul3x3-j next-j i0))))

          (mul3x3-i (j0 i)
            (:if (:= i 3) 
               :#t
               (:let ((next-i (:+ i 1)))
                 (mul3x3-j j0 i)
                 (mul3x3-i j0 next-i)))))

         (mul3x3-i 0 0))))





