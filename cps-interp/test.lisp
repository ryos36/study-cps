(load "cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'parse-cps)
(defparameter *debug-mode* t)

(set-test-files '(3))
(do-test)
