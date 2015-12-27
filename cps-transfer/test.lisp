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

(set-test-files '(7 12 15 16 17 18 19 20 21 22))

(defparameter *env* (make-exit-continuous))
(do-test)

#|
(print
  (fbind-transfer `(func_name (a b c d) (:* (:+ a b) (:- c d))) (cdr *env*)))

(print
  (do-lisp-to-cps `(:fix ((func0 (a b c d) (:* (:+ a b) (:- c d)))
                        (func1 (x y z) (:- (:* x y) (:+ z g))))
                       (:+ 3 5))
                       (cdr *env*)))
(print
  (do-lisp-to-cps `(:+ 3 5)
                       (cdr *env*)))
|#

#|
(let ((expr '(:FIXS ((x-inner-func-name (x-clouse-result) CONT))
                    (:NEQ? (x-cont-result-sym :#f)
                           ((:APP x-inner-func-name (TRUE-CLOUSE))
                            (:APP x-inner-func-name (FALSE-CLOUSE)))))))
  (let ((e0 (pickup-list expr 'CONT))
        (e1 (pickup-list expr 'TRUE-CLOUSE))
        (e2 (pickup-list expr 'FALSE-CLOUSE)))

    (print `(,e0 ,e1 ,e2))
    (flet ((ee0 (a0) (setf (car e0) a0) expr)
           (ee1 (a0) (setf (car e1) a0) expr)
           (ee2 (a0) (setf (car e2) a0) expr))

      (ee0 'put-into-cont)
      (ee1 'put-into-true-clouse)
      (ee2 'put-into-false-clouse)
  (print expr))))
|#
