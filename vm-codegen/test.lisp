(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

(load "package.lisp")
(load "vm-codegen.lisp" )
(load "heap-parser.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :vm-codegen)
(use-package :cps-test)

(setf finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
(setf codegen (make-instance 'vm-codegen:vm-codegen :live-variables-finder finder :sym-name "label"))
(setf *test-env* (make-new-env codegen '()))

(defun cps-parse-one (cps-expr env)
  (let* ((finder-env (make-new-env finder '() '()))
         (result (cps-parse finder cps-expr finder-env))
         (codegen-env (make-new-env codegen '()
                                    (copy-tree `((:live-vars ,result)
                                                 (:codegen
                                                   (:register ,(make-list (max-n codegen)))
                                                   (:app-info)))))))

    (cps-parse codegen cps-expr codegen-env)
    (create-initialize-codes codegen)
    (get-final-codes codegen)))

(defparameter *test-script-dir* "./cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defparameter *test-insn-view* t)

(defun cps-gensym-reset ()
  (setf (slot-value codegen 'sym-no) 0)
  (reset-codes codegen))

(defparameter *test-reset-func* #'cps-gensym-reset)

;(set-test-files '("32" "42" "29" "41"))
(set-test-files '("100" "7" (1 . 3)))
(do-test)
