((:fixh ((f0 (r) (:+ (ex-f0 r) (t) ((:app ex-f1 (t))))))
   (:+ (ex-a ex-b) (sym0) (
     (:fixh ((g0 (k0 b1) (:* (ex-g0 sym0) (sym1) (
          (:app k0 (sym1)))))
             (g1 (k1 b2) (:- (ex-g1 sym0) (sym4) (
          (:app k1 (sym4))))))
        (:- (ex-c ex-d) (sym3) (
           (:app g0 (ex-k sym3)))))))))
