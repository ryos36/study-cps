(load "package.lisp")
(load "cps-parser.lisp")
(load "utils.lisp")

(load "package.lisp")
(load "free-variable-finder.lisp")
(load "closure-converter.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :cps-free-variable-finder)
(use-package :cps-closure-converter)
(use-package :cps-test)

(setf conv (make-instance 'cps-closure-converter:closure-converter :sym-name "k-sym"))
(setf *test-env* (make-new-env conv '()))
(setf grv nil)
;(defparameter *test-save* nil)

(defun cps-parse-one (cps-expr env)
  (let* ((op (car cps-expr))
         (rv (if (eq :fixh op)
               (cps-fixh conv cps-expr env)
               (if (eq :fixs op)
                 (cps-fixs conv cps-expr env)
                 (cps-parse conv cps-expr env)))))
    (setf grv rv)
    rv))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-script-dir* "../vm-codegen/cps-script/" )
(defparameter *test-script-dir* "./cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value conv 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

;(cps-parse-one '(:fixs ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) *env*)

(set-test-files '((14 . 24) (26 . 42) (50 . 57)))
(set-test-files '((1 . 7)))
(do-test)

