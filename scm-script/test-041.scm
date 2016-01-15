((:define (mul3x3 a b c)
   (:fix ((mul3x3-ji (j0 i0)
           (:let ((j-start (:* i0 3))
                  (i-start j0))
             (:let ((a-ji (recored-ref a j-start))
                    (b-ji (recored-ref b i-start)))
                   (record-set! c (:+ j-start i-start) (:* a-ji b-ji)))))

          (mul3x3-j (j i0)
            (:if (:= j 3) 
               :#t
               (mul3x3-ji j i0))
            (mul3x3-j (:+ j 1) i0))

          (mul3x3-i (j0 i)
            (:if (:= i 3) 
               :#t
               (mul3x3-j j0 i))
            (mul3x3-i j0 (:+ i 1))))

         (mul3x3-i 0 0))))





