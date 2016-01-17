((:fix ((fact (k n)
          (:fix ((fact0 (k n rv)
                    (:if (:= n 0)
                         rv
                         (fact0 k (:- n 1) (:* n rv)))))
                 (fact0 k n 1))))

        (fact 10)))

