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
(defun +-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:+ (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))

;----------------------------------------------------------------
(defun if-transfer (expr env))
(defun fix-transfer (expr env))
(defun user-func-transfer (expr env))

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

    (case (op)
      (:if (if-trasfer expr env))
      (:fix (fix-transfer expr env)) 
      (otherwise (user-func-transfer expr env)))))))

