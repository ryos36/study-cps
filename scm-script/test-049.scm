((:fix ((fact (k n)
           (:if (:= n 0)
                1
                (:* n (fact k (:- n 1))))))
        (fact 10))

 (:fix ((fact0 (k n rv)
          (:if (:= n 0)
               rv
               (fact0 k (:- n 1) (:* n rv))))

         (fact (k n)
           (fact0 k n 1)))

        (fact 10)))

