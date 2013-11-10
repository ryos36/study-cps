;
; Dynamic Scope Test
;
((:define x 3)
 x
 (:define (f a b) (:- (:+ a b) x))
 (f 4 5)
 (:define (g x y) (:+ x (f x y)))
 (g 7 8))

