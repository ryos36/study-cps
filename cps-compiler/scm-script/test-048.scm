((:let ((y (:heap 19201738)))
   (:fix ((clear-bit0 (x)
            (:<< (:>> x 1) 1))
  
          (get-bit0 (x)
            (:- x (clear-bit0 x)))
  
          (xor-bit (b0 b1)
            (:if (:= b0 b1) 0 1))
  
          (xor0 (bit-n n x0 x1 rv)
            (:if (:= 0 bit-n) rv
                (:let ((x0-bit0 (get-bit0 x0))
                       (x1-bit0 (get-bit0 x1)))
                  (xor0
                    (:- bit-n 1)
                    (:+ n 1)
                    (:>> x0 1)
                    (:>> x1 1)
                    (:+ rv (:<< (xor-bit x0-bit0 x1-bit0) n))))))
  
          (xor (bit-n x0 x1)
             (xor0 bit-n 0 x0 x1 0))

          (rand ()
            (:let ((y0 (:record-ref y 0)))
              (:let ((y1 (xor 32 y0 (:<< y0 13))))
                (:let ((y2 (xor 32 y1 (:>> y1 17))))
                  (:let ((y3 (xor 32 y2 (:<< y2 15))))
                    (:record-set! y3 0 y)
                    y3))))))

         (rand)
         (rand)
         (rand)
         (:exit (rand)))))

