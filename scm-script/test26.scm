((:+ 3 (:heap (:+ 0 2) (:* 1 3) (:- 2 5) 3))
 (:record-set! r (:+ 0 4) (:* 10 y))
 (:* 
   (:record-ref r 1)
   (:record-ref r 2)))
