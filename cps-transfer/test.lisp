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

(set-test-files '(7 15 16))
(set-test-files '(7 15 16 17 18))
(set-test-files '(10 17 18 19 20))

(defparameter *env* (make-exit-continuous))
(do-test)
