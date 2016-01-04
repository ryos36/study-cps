(load "free-variable-finder.lisp")
(setf finder (make-instance 'free-variable-finder))
(setf env (make-new-env finder '()))
(print `(:cps-fixs
  (cps-fixs conv '(:fixs ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) env)))

(print env)

