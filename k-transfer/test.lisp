(load "closure-converter.lisp")
(load "../test-lisp/test.lisp")

(setf conv (make-instance 'closure-converter))
(setf *env* (make-new-env conv '()))
(setf grv nil)

(defun cps-parse-one (cps-expr env)
  (let* ((op (car cps-expr))
         (rv (if (eq :fixh op)
               (cps-fixh conv cps-expr env)
               (if (eq :fixs op)
                 (cps-fixs conv cps-expr env)
                 (cps-parse conv cps-expr env)))))
    (setf grv rv)
    rv))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value conv 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

;(cps-parse-one '(:fixs ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) *env*)

(set-test-files '("30" (14 . 29) (31 . 39)))
(do-test)

