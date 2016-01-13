(load "cps-block-analyzer.lisp" )
(load "../test-lisp/test.lisp")

(setf analyzer (make-instance 'cps-block-analyzer))
(setf *env* (make-new-env analyzer '()))

(defun cps-parse-one (cps-expr env)
  (cps-parse analyzer cps-expr env))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value analyzer 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("24" (14 . 22)))
(do-test)
