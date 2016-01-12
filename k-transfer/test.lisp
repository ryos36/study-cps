(load "closure-converter.lisp")
(load "../test-lisp/test.lisp")

(setf conv (make-instance 'closure-converter))
(setf *env* (make-new-env conv '()))
(setf grv nil)

(defun cps-parse-one (cps-expr env)
  (let* ((op (car cps-expr))
         (rv (if (eq :fixh op)
               (cps-fixh conv cps-expr env)
               (cps-fixs conv cps-expr env))))
    (setf grv rv)
    rv))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
;(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value conv 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

;(cps-parse-one '(:fixs ((c (r) (:+ (x r) (t) ((:app k (t)))))) (:app g (c x))) *env*)

;(set-test-files '((1 . 3) (6 . 16)))
(set-test-files '((14 . 22)))
(do-test)

#|
(setf grv0 (caar (cadddr (caar (cadddr (caar (cadddr (caar (cadddr (caddr (caadr grv)))))))))))

(setf grv1 (cdar (cadddr (car (cadddr (car (cadddr (caar (cadddr grv0)))))))))

(print grv1)
(print "================")
(print (cadadr (caadr (cddadr grv1))))
(setf ln (cadr (caadr (cddadr grv1))))
(print ln)
(setf (cdr ln) 9)
;(print (cadadr (caadr (cddadr grv1))))
(print grv1)
|#
