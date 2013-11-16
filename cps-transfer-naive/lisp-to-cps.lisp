;----------------------------------------------------------------
(defparameter *env* nil)
(defparameter *debug-mode* nil)
(defparameter *transfer-table* nil)

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
(defun cps-gensym () (gensym))

;----------------------------------------------------------------
(load "primitive.lisp")

;----------------------------------------------------------------
(defun exit-transfer (expr env)
  (let ((arg0 (cadr expr))

        (exit-arg-sym (cps-gensym)))

    (format t "arg0:~a~%" arg0)
    (do-lisp-to-cps arg0 (cons exit-arg-sym
      (copy-tree
        `(:exit (,exit-arg-sym) () ()))))))

(defun if-transfer (expr env)
  (let ((condition-expr (cadr expr))
        (true-clouse (caddr expr))
        (false-clouse (cadddr expr))

        (condition-sym (cps-gensym))

        (result-sym (car env)) ; not use
        (continuation (cdr env)))

    (do-lisp-to-cps condition-expr (cons condition-sym
      (copy-tree `(:neq (,condition-sym :#f) () (
         ,(do-lisp-to-cps true-clouse (cons nil continuation))
         ,(do-lisp-to-cps false-clouse (cons nil continuation)))))))))

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
        (args (cadr expr))

        (continuation-sym (cdr env)))

    (let ((cps-new-args (cons continuation-sym args)))
      (copy-tree `(:app ,func-name ,cps-new-args)))))


;----------------------------------------------------------------
(setf *transfer-table* (make-hash-table))
(setf (gethash ':+ *transfer-table*) #'+-two)

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
      (:if (if-trasfer expr env))
      (:fix (fix-transfer expr env)) 
      (:exit (exit-transfer expr env))
      (otherwise (user-func-call-transfer expr env)))))))

