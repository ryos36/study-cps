(define (my-func b c k)
 (+ b c))

(fix [my-func (b c k)
   (fix [c (t) (app k (t)]
      (+ [b c] [t] [app c (t)]))))


(define (my-func f0 b f1 c k)
 (+ (f0 b) (f1 c)))

(fix [my-func (f0 b f1 c k)
  (fix [c0 (f0b)
      (fix [c1 (f1c)
          (+ [f0b f1c] [result]
              [(app k (result))])]
              (app f1 (b c1)))])]
    (app f0 (b c0)))

