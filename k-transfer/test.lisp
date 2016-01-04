(load "closure-converter.lisp")
(setf conv (make-instance 'closure-converter))
(setf env (make-new-env conv '()))
(print `(:cps-fixs
  (cps-fixs conv '(:fixs ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) env)))

(print `(:cps-fixh
  ,(cps-fixh conv '(:fixh ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) env)))
