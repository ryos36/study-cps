(load "package.lisp")
(load "vmgen.lisp")
(load "vmc-to-c-source.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-vmgen)
(use-package :cps-vmc-to-c-source)
(use-package :cps-test)

(setf vmgen (make-instance 'vmgen))
(setf converter (make-instance 'vmc-to-c-source :vmgen vmgen))
(setf *test-env* nil)

(defun vmgen-one (code-tagged-list env)
  (mapcar #'(lambda (expr)
              (convert converter expr))
          (cdr code-tagged-list))
  (get-codes vmgen)
  ;(write-out-labels vmgen *standard-output*)
  )

(defparameter *test-script-dir* "./codes/" )
(defparameter *test-ext* ".vmc")
(defparameter *test-parse-func* #'vmgen-one)
(defparameter *debug-mode* nil)
(defparameter *test-src-insn-view* t)
(defparameter *test-insn-view* t)

;(set-test-files '("32" "42" "29" "41"))
(set-test-files '("1" (1 . 3)))
(do-test)
