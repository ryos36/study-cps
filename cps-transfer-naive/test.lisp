(load "lisp-to-cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../scm-script/" )
(defparameter *test-ext* ".scm")
(defparameter *test-parse-func* #'do-lisp-to-cps)

(set-test-files 7)
(set-test-files '(7))

(defparameter *env* (make-exit-continuous))
(do-test)
