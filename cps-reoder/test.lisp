(load "cps-reoder.lisp" )
(load "../test-lisp/test.lisp")

(setf finder (make-instance 'cps-reoder))
(setf *env* (make-new-env finder '()))

(defun cps-parse-one (cps-expr env)
  (cps-parse finder cps-expr env))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value finder 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)


(set-test-files '("24" (14 . 22)))
(do-test)
