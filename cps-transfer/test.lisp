(load "package.lisp")
(load "lisp-to-cps.lisp")
(load "make-cxr-route.lisp")
(load "primitive.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-transfer)
(use-package :cps-test)

(defparameter *test-script-dir* "../scm-script/" )
(defparameter *test-ext* ".scm")
(defparameter *test-parse-func* #'do-lisp-to-cps)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)
;(defparameter *test-save* nil)

(defun cps-gensym-reset ()
  (cps-gensym 0))

(defparameter *test-reset-func* #'cps-gensym-reset)


(defparameter *transfer-table* (make-transfer-table))

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))

;(format t "~a~%" last-arg)
;(format t "transfer-table:~a~%" *transfer-table*)

(set-test-files '((1 . 59)))

(setf use-exit-primitive nil)
(defparameter *test-env* (make-exit-continuous use-exit-primitive))
(do-test)

