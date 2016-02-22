(load "package.lisp")
(load "csf-resources.lisp")
(load "acps-environment.lisp")
(load "acps-to-acps.lisp")

(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :acps-to-acps)
(use-package :acps-environment)
(use-package :cps-test)

(setf cps->acps (make-instance 'cps-parser :sym-name "k-sym" :add-attribute t))
(setf conv (make-instance 'acps-to-acps))
(setf acps-env (make-instance 'acps-environment))
(setf *test-env* (make-new-env cps->acps '()))

(defun cps-parse-one (cps-expr env)
  (let* ((rv (cps-parse cps->acps cps-expr env))
         (rv0 (acps->acps conv rv acps-env)))
    rv0))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-script-dir* "../vm-codegen/cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value cps->acps 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("1" (1 . 13) (114 . 115)))
(do-test)

