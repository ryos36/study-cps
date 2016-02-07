(load "package.lisp")
(load "vmgen.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-vmgen)
(use-package :cps-test)

(setf vmgen (make-instance 'vmgen))
(setf *test-env* nil)

(defun vmgen-one (cps-expr env)
  (let ((*xstandard-output* nil))
    (primitive-+ vmgen :r0 1 :r0)))

(defparameter *test-script-dir* "./vm-code/" )
(defparameter *test-ext* ".vmc")
(defparameter *test-parse-func* #'vmgen-one)
(defparameter *debug-mode* nil)
(defparameter *test-insn-view* t)

;(set-test-files '("32" "42" "29" "41"))
(set-test-files '("1" (1 . 3)))
(do-test)
