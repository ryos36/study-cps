(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")

(load "package.lisp")
(load "cps-live-variables-finder.lisp" )

(use-package :cps-parser)
(use-package :cps-live-variables-finder)

(setf finder (make-instance 'cps-live-variables-finder))
(setf *test-env* (make-new-env finder '() '()))

#|
(print 
  (macroexpand-1
    '(create-get-vars-list-method get-vars (parser cps-live-variables-finder)
                                  '(:declare :use :live))))

(let ((tag-list '(:declare :use :live)))
  (mapc #'(lambda (tag)
    (let* ((func-name-string (format nil "GET-~a-TAGGED-LIST" tag))
           (func-name (intern func-name-string)))
      (print (macroexpand-1
               `(create-get-tagged-list-method ,func-name (parser cps-live-variables-finder) ,tag)))))
        tag-list))
|#

(print 
  (get-vars finder '(:op (:declare x y z)
                         (:use a b c)
                         (:live a b c))))
(print 
  (get-declare-tagged-list finder '(:op (:declare x y z)
                                         (:use a b c)
                                         (:live a b c))))
