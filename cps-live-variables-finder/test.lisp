(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")

(load "package.lisp")
(load "cps-live-variables-finder.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :cps-live-variables-finder)
(use-package :cps-test)

(setf live-variables-finder (make-instance 'cps-live-variables-finder))
(setf *test-env* (make-new-env live-variables-finder '() '()))

(defun cps-parse-one (cps-expr env)
  (cps-parse live-variables-finder cps-expr env)
  )

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value live-variables-finder 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("44"))
(do-test)
