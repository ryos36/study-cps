(load "lisp-to-cps.lisp")
(load "../test-lisp/test.lisp")

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

(set-test-files '("40"(1 . 39)))

(defparameter *env* (make-exit-continuous))
(do-test)

