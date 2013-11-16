(load "mini-lisp.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../scm-script/" )
(defparameter *test-ext* ".scm")
(defparameter *test-parse-func* #'parse-mini-lisp)

(set-test-files 6)
(do-test)
