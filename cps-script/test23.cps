((:fixh ((f0 (r) (:+ (ex-f0 r) (t) ((:app ex-f1 (t)))))
         (f1 (kf0 a0 a1) (:+ (a0 ex-f2) (sym4) (
            (:app kf0 (sym4))))))
   (:+ (ex-a ex-b) (sym0) (
     (:fixh ((g0 (k0 b1) (:* (ex-g0 sym0) (sym1) (
          (:+ (b1 sym1) (ex-g1) (
           (:app k0 (ex-g1)))))))
             (g1 (k2 b2) (:+ (ex-g1 ex-g2) (sym5) (
          (:app f0 (sym5))))))  
        (:- (ex-c ex-d) (sym3) (
           (:app g0 (ex-k sym3)))))))))
