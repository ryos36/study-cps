;----------------------------------------------------------------
(defparameter *env* nil)
(defparameter *debug-mode* nil)
(defparameter *transfer-table* nil)
(defparameter *cps-gensym-debug* t)

;----------------------------------------------------------------
(defun l ()
  (load "lisp-to-cps.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (null expr)
      (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
  
(defun terminal-transfer (expr env)
  (let ((arg0 expr)

        (result-sym (car env))
        (continuation (cdr env)))

    (copy-tree `(:id (,arg0) (,result-sym) (,continuation)))))

;----------------------------------------------------------------
(let ((no 0))
 (defun cps-gensym () 
   (if *cps-gensym-debug*
     (intern (format nil "sym~a" (incf no)))
     (gensym))))

;----------------------------------------------------------------
(load "primitive.lisp")

;----------------------------------------------------------------
(defun make-exit-continuous ()
  (let ((r (cps-gensym))
        (k (cps-gensym)))
    (copy-tree `(,r . 
                 (:exit (,r) () ())
                 ))))

;----------------------------------------------------------------
(defun exit-transfer (expr env)
  (let ((arg0 (cadr expr))

        (exit-arg-sym (cps-gensym)))

    (do-lisp-to-cps arg0 (cons exit-arg-sym
      (copy-tree
        `(:exit (,exit-arg-sym) () ()))))))

(defun if-transfer (expr env)
  (let ((condition-expr (cadr expr))
        (true-clouse (caddr expr))
        (false-clouse (cadddr expr))

        (condition-sym (cps-gensym))

        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps condition-expr (cons condition-sym
      (copy-tree `(:neq (,condition-sym :#f) () (
         ,(do-lisp-to-cps true-clouse (cons result-sym continuation))
         ,(do-lisp-to-cps false-clouse (cons result-sym continuation)))))))))

;----------------------------------------------------------------
;(func-name (arg*) expr)

(defun func-transfer (fbind)
  (let ((func-name (car fbind))
        (args (cadr fbind))
        (func-expr (caddr fbind))

        (kont-sym (cps-gensym))
        (result-sym (cps-gensym)))

    (let ((cps-new-args (cons kont-sym args)))
      (copy-tree `(,func-name ,cps-new-args ,(do-lisp-to-cps func-expr (cons result-sym `(:app ,kont-sym (,result-sym)))))))))

;----------------------------------------------------------------
(defun fix-transfer (expr env)
  (let ((fbinds (cadr expr))
        (fix-expr (caddr expr))

        (result-sym (car env)) ; not use
        (continuation (cdr env)))

    (let ((cps-binds (mapcar #'(lambda (fbind) (func-transfer fbind)) fbinds)))
      (copy-tree `(:fix ,cps-binds
                        ,(do-lisp-to-cps fix-expr env))))))


;----------------------------------------------------------------
(defun user-func-call-transfer (expr env)
  (let ((func-name (car expr))
        (args (cdr expr))

        (inner-func-name (cps-gensym))
        (inner-func-arg0 (cps-gensym))

        (continuation (cdr env)))

    (let ((cps-new-args (cons inner-func-name args)))
      (copy-tree `(:fix ((,inner-func-name (,inner-func-arg0)
                                           (,continuation ,inner-func-arg0)))
        (:app ,func-name ,cps-new-args))))))


;----------------------------------------------------------------
(defun make-transfer-table ()
  (let ((table (make-hash-table)))
    (map nil #'(lambda (x) 
                 (let ((op (car x))
                       (func (cdr x)))
                   (setf (gethash op table) func)))
         `((:+ . ,#'+-two)
           (:- . ,#'--two)
           (:>> . ,#'>>-two)
           (:<< . ,#'<<-two)

           (:> . ,#'>-two)
           (:< . ,#'<-two)
           (:>= . ,#'>=-two)
           (:<= . ,#'<=-two)
           (:= . ,#'=-two)))
    table))

;----------------------------------------------------------------
(defun do-lisp-to-cps (expr env)
  (if *debug-mode*
    (format t "expr:~s~%" expr))
  (if (terminal-p expr)
    (terminal-transfer expr env)
    (let* ((op (car expr))
           (transfer (gethash op *transfer-table*)))
      (if transfer
        (funcall transfer expr env)

    (case op
      (:if (if-transfer expr env))
      (:fix (fix-transfer expr env)) 
      (:exit (exit-transfer expr env))
      (otherwise (user-func-call-transfer expr env)))))))

