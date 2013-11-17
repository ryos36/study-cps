(load "lisp-to-cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../scm-script/" )
(defparameter *test-ext* ".scm")
(defparameter *test-parse-func* #'do-lisp-to-cps)
(defparameter *debug-mode* nil)
(defparameter *cps-gensym-debug* t)

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))

(format t "~a~%" last-arg)

(set-test-files 8)
(set-test-files '(9 10))

(defparameter *env* (make-exit-continuous))
(do-test)
