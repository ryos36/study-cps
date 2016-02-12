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
(load "../vm-codegen/package.lisp")
(load "../vm-codegen/vm-codegen.lisp" )
(load "../vm-codegen/heap-parser.lisp" )

;----------------------------------------------------------------
(use-package :cps-transfer)
;(use-package :cps-eta-reduction)
;(use-package :cps-parser)
(use-package :cps-free-variable-finder)
(use-package :cps-closure-converter)
(use-package :cps-spill)
(use-package :vm-codegen)

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
(setf spill (make-instance 'cps-spill:cps-spill :sym-name "s-sim" :max-n 10))

(defun cps-spill-parse-one (spill cps-expr env)
  (let* ((finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
         (finder-env (cps-parser:make-new-env finder '() '()))
         (result (cps-parser:cps-parse finder cps-expr finder-env))
         (spill-env (cps-parser:make-new-env spill '() 
                                  (copy-tree `((:live-vars ,@result)
                                               (:spill
                                                 (:used )
                                                 (:duplicate 0)
                                                 (:spill-out ))))
                                  )))

    (cps-parser:cps-parse spill cps-expr spill-env)))

;----------------------------------------------------------------
(defparameter *codegen* nil)

(defun cps-codegen-parse-one (cps-expr env)
  (let* ((finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
         (codegen (make-instance 'vm-codegen:vm-codegen :live-variables-finder finder :sym-name "label"))
         (finder-env (cps-parser:make-new-env finder '() '()))
         (result (cps-parser:cps-parse finder cps-expr finder-env))
         (codegen-env (cps-parser:make-new-env codegen '()
                                    (copy-tree `((:live-vars ,result)
                                                 (:codegen
                                                   (:register ,(make-list (max-n codegen)))
                                                   (:app-info)))))))
    (setf *codegen* codegen)

    (cps-parser:cps-parse codegen cps-expr codegen-env)
    (create-initialize-codes codegen)
    (get-final-codes codegen)))

;----------------------------------------------------------------
(defun print-expr (expr env)
  (print `(:expr ,expr))
  expr)
;----------------------------------------------------------------
(setf func-env-pair `((,#'do-lisp-to-cps . ,(make-exit-continuous use-exit-primitive))
                      (,#'cps-eta-reduction:walk-cps . ,(cps-eta-reduction:make-env))
                      ((,#'cps-parser:cps-parse . ,reorder) .  ,(cps-parser:make-new-env reorder '()))
                      ((,#'cps-parser:cps-parse . ,conveter) .  ,(cps-parser:make-new-env conveter '()))
                      ((,#'cps-spill-parse-one . ,spill) . nil)
                      (,#'print-expr)
                      (,#'cps-codegen-parse-one . nil)
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

