(:define gv0 32)
(:define gv (:+ gv0 32))
(:fix ((e (z) (:* (:+ gv gv0)
                  (:fix ((f (a) (:* (:+ a z)
                                    (:fix ((g (b) (:* z
                                                      (:fix ((h (c) (:+ c gv)))
                                                            (h 5)))))
                                          (g 6)))))
                        (f 7)))))
      (:exit (e 8)))

