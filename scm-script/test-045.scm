((:define (mul3x3 a b c)
   (:fix ((mul3x3-ji (j0 i0)
           (:let ((j-start (:* i0 3))
                  (i-start j0))
             (:let ((a-ji (:record-ref a j-start))
                    (b-ji (:record-ref b i-start)))
                   (:record-set! c (:+ j-start i-start) (:* a-ji b-ji)))))

          (mul3x3-j (j i0)
            (:fix ((mul3x3-j-i0 (j)
                     (:if (:= j 3) 
                        :#t
                        (:let ((next-j (:+ j 1)))
                          (mul3x3-ji j i0)
                          (mul3x3-j-i0 next-j)))))
                  (mul3x3-j-i0 j)))

          (mul3x3-i (i)
            (:if (:= i 3) 
               :#t
               (:let ((next-i (:+ i 1)))
                 (mul3x3-j i)
                 (mul3x3-i next-i)))))

         (mul3x3-i 0 0))))
