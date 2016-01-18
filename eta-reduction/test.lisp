(load "cps.lisp")
(load "../test-lisp/test.lisp")

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'walk-cps)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)

(let ((av (argv)))
  (setf last-arg (elt av (- (length av) 1))))

;(format t "~a~%" last-arg)
;(format t "transfer-table:~a~%" *transfer-table*)

; test0 includes string , so ignore here

(set-test-files '("51" "50" "42" "37" "40" "38" "31" (1 . 28)))

(defparameter *env* (make-env))
(do-test)
