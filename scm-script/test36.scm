((:fix ((fibo (n)
              (:if (:= n 0) 1
                   (:if (:= n 1) 1
                        (:+ (fibo (:- n 1)) (fibo (:- n 2)))))))
       (fibo 8)))

