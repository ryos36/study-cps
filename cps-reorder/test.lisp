(load "cps-reorder" )
(load "../test-lisp/test.lisp")

(setf reorder (make-instance 'cps-reorder))
(setf *env* (make-new-env reorder '()))

(defun cps-parse-one (cps-expr env)
  (cps-parse reorder cps-expr env))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value reorder 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(set-test-files '("25" (14 . 22)))
(do-test)

#|
(setf reorder (make-instance 'cps-reorder))
;(setf (reorder new-order) '(a b c))
;(setf (get-new-order reorder) '(a b c))
;(reset reorder)
(print `(,(get-new-order reorder)))
(pop-cps-expr reorder)
|#
