(:fix ((fact0 (n rv)
          (:if (:= n 0)
               rv
               (fact0 (:- n 1) (:* n rv))))

       (fact (n)
             (fact0 n 1)))

      (fact 10))
