((:fix ((fibo-2 (n a-1 a-2)
              (:if (:= n 0)
                   a1
                   (fibo-2 (:- n 1) (:+ a-1 a-2) a-1))) 
        (fibo (n)
              (fibo-2 n 1 0)))
       (fibo 8)))

