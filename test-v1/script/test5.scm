(define f
  (let ((v0 129))
    (define (f0 a) (if a
                     (let ((temp 0))
                       (define (g x) (display v0))
                       g)
                     (display v0)))
    f0
    ))

(f #f)
(define g (f #t))
(let ((v0 113))
  (g))
