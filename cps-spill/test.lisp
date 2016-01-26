(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")

(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

(load "package.lisp")
(load "cps-spill.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
;(use-package :cps-spill)
(use-package :cps-test)

(setf spill (make-instance 'cps-spill:cps-spill))
(setf finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
(setf *test-env* (make-new-env spill '()))

(defun cps-parse-one (cps-expr env)
  (let* ((finder-env (make-new-env finder '() '()))
         (result (cps-parse finder cps-expr finder-env))
         (spill-env (make-new-env spill '() 
                                  (copy-tree `((:live-vars ,@result)
                                               (:spill
                                                 (:used )
                                                 (:duplicate )
                                                 (:spill-out ))))
                                  )))

    (cps-parse spill cps-expr spill-env)
    ))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value spill 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("26" (14 . 22)))
(do-test)
