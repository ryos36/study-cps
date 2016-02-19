(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")

(load "../resource-scheduler/package.lisp")
(load "../resource-scheduler/resource-scheduler.lisp")

(load "package.lisp")
(load "vm-scheduler.lisp")
(load "cps-block-analyzer.lisp")
(load "cps-reorder.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :cps-reorder)
(use-package :cps-test)
(use-package :vm-scheduler)

(setf reorder (make-instance 'cps-reorder))
(setf *test-env* (make-new-env reorder '()))

(defun cps-parse-one (cps-expr env)
  (cps-parse reorder cps-expr env))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value reorder 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("25" "26" (14 . 22)))
(do-test)

;(setf s (make-instance 'vm-scheduler))
;(print s)
;;(print (resources s))
