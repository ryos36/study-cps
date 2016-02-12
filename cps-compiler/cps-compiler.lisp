(load "../cps-transfer/package.lisp")
(load "../cps-transfer/lisp-to-cps.lisp")
(load "../cps-transfer/make-cxr-route.lisp")
(load "../cps-transfer/primitive.lisp")

(load "../eta-reduction/package.lisp")
(load "../eta-reduction/cps.lisp")
(load "../eta-reduction/optimize.lisp")

(use-package :cps-transfer)
;(use-package :cps-eta-reduction)

(defun cps-gensym-reset ()
  (cps-gensym 0))

(defparameter *transfer-table* (make-transfer-table))

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))


(setf use-exit-primitive nil)
(setf *env* (make-exit-continuous use-exit-primitive))

(defun my-test (expr env) expr)

(setf func-env-pair `((,#'my-test . ())
                      (,#'do-lisp-to-cps . ,(make-exit-continuous use-exit-primitive))
                      (,#'cps-eta-reduction:walk-cps . ,(cps-eta-reduction:make-env))))

(print
    (do-lisp-to-cps '(:+ 3 5) *env*))
(print 
  (cps-eta-reduction:walk-cps
    (do-lisp-to-cps '(:+ 3 5) *env*)
    (cps-eta-reduction:make-env)))

(labels ((proc-loop (func-env-pair0 expr)
            (if (null func-env-pair0) expr
              (let ((func (caar func-env-pair0))
                    (env (cdar func-env-pair0)))
              (proc-loop (cdr func-env-pair0) (funcall func expr env))))))

  (print (proc-loop func-env-pair '(:+ 4 6))))

