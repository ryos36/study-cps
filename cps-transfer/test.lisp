(load "lisp-to-cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../scm-script/" )
(defparameter *test-ext* ".scm")
(defparameter *test-parse-func* #'do-lisp-to-cps)
(defparameter *debug-mode* nil)
(defparameter *cps-gensym-debug* t)
(defparameter *transfer-table* (make-transfer-table))
;(defparameter *test-save* nil)

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))

;(format t "~a~%" last-arg)
;(format t "transfer-table:~a~%" *transfer-table*)

(set-test-files '(1 2 3 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29))

(defparameter *env* (make-exit-continuous))
(do-test)

