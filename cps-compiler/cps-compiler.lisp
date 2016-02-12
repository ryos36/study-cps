;----------------------------------------------------------------
(load "../cps-transfer/package.lisp")
(load "../cps-transfer/lisp-to-cps.lisp")
(load "../cps-transfer/make-cxr-route.lisp")
(load "../cps-transfer/primitive.lisp")

;----------------------------------------------------------------
(load "../eta-reduction/package.lisp")
(load "../eta-reduction/cps.lisp")
(load "../eta-reduction/optimize.lisp")

;----------------------------------------------------------------
(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

(load "../k-transfer/free-variable-finder.lisp")
(load "../k-transfer/closure-converter.lisp")

;----------------------------------------------------------------
(load "../resource-scheduler/package.lisp")
(load "../resource-scheduler/resource-scheduler.lisp")

;----------------------------------------------------------------
(load "../cps-reorder/package.lisp")
(load "../cps-reorder/vm-scheduler.lisp")
(load "../cps-reorder/cps-block-analyzer.lisp")
(load "../cps-reorder/cps-reorder.lisp" )

;----------------------------------------------------------------
(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

;----------------------------------------------------------------
(load "../cps-spill/package.lisp")
(load "../cps-spill/cps-spill.lisp" )

;----------------------------------------------------------------
(use-package :cps-transfer)
;(use-package :cps-eta-reduction)
;(use-package :cps-parser)
(use-package :cps-free-variable-finder)
(use-package :cps-closure-converter)

(defun cps-gensym-reset ()
  (cps-gensym 0))

(defparameter *transfer-table* (make-transfer-table))

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))


(setf use-exit-primitive nil)
(setf *env* (make-exit-continuous use-exit-primitive))

;(setf codegen (make-instance 'vm-codegen:vm-codegen :live-variables-finder finder :sym-name "label"))

;(use-package :cps-parser)
;(setf *test-env* (make-new-env codegen '()))

(setf reorder (make-instance 'cps-reorder:cps-reorder :sym-name "ro-sym"))
;----------------------------------------------------------------
(setf conveter (make-instance 'cps-closure-converter:closure-converter :sym-name "k-sym"))

;----------------------------------------------------------------
(setf func-env-pair `((,#'do-lisp-to-cps . ,(make-exit-continuous use-exit-primitive))
                      (,#'cps-eta-reduction:walk-cps . ,(cps-eta-reduction:make-env))
                      ((,#'cps-parser:cps-parse . ,reorder) .  ,(cps-parser:make-new-env reorder '()))
                      ((,#'cps-parser:cps-parse . ,conveter) .  ,(cps-parser:make-new-env conveter '()))
                      ))

;----------------------------------------------------------------
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
                (if (listp func)
                  (let ((method (car func))
                        (instance (cdr func)))
                    (proc-loop (cdr func-env-pair0) (funcall method instance expr env)))

                  (proc-loop (cdr func-env-pair0) (funcall func expr env)))))))

  (print (proc-loop func-env-pair '(:+ 4 6))))

