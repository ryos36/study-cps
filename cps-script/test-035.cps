((:fixh ((rx (k r2) (:+ (r2 r1) (rr) ((:app k (rr))))))
    (:+ (n r1) (r4) (
      (:fixh ((f0 (k a b c) (:+ (a b) (r0) (
          (:- (r0 r1) (r3) (
          (:* (r4 c) (r2) (
          (:app k (r3))))))))))
   (:app f0 (k 1 2 3)))))))

