(load "cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'parse-cps)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* nil)

(set-test-files '("12" (0 . 3) (6 . 11)))
(do-test)
