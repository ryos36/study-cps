((:let ((y (:heap 19201738)))
   (:fix ((xor0 (bit-n n x0 x1 rv)
            (:if (:= 0 bit-n) rv
                (:let ((x0-bit0 (get-bit0 x0))
                       (x1-bit0 (get-bit0 x1)))
                  (xor0
                    (:- bit-n 1)
                    (:+ n 1)
                    (:>> x0 1)
                    (:>> x1 1)
                    (:+ rv (:<< (xor-bit x0-bit0 x1-bit0) n))))))

         (xor1 (bit-n n x0 x1 rv)
            (:if (:= 0 bit-n) rv
                (:let ((x0-bit0 (get-bit0 x0))
                       (x1-bit0 (get-bit0 x1))
                       (bit-n-1 (:- bit-n 1))
                       (n+1     (:+ n 1))
                       (x0>>1   (:>> x0 1))
                       (x1>>1   (:>> x1 1)))
                  (xor1
                    bit-n-1
                    n+1
                    x0>>1
                    x1>>1
                    (:+ rv (:<< (xor-bit x0-bit0 x1-bit0) n))))))
  
          (xor (bit-n x0 x1)
             (xor0 bit-n 0 x0 x1 0)))

         (:exit (xor 32 3 5)))))

