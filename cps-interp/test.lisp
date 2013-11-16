(load "cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'parse-cps)
(defparameter *debug-mode* nil)

(set-test-files '(0 1 2 3))
(set-test-files 2)
(set-test-files '(3))
(do-test)
