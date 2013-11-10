;----------------------------------------------------------------
; essential

(defun lisp-if (expr env)
  (let ((condition-s (cadr expr))
        (true-clouse (caddr expr))
        (false-clouse (cadddr expr)))
    (let ((cond-result (parse-mini-lisp condition-s env)))
      (if (not (eq :#f cond-result))
        (parse-mini-lisp true-clouse env)
        (if false-clouse 
          (parse-mini-lisp false-clouse env))))))

(defun lisp-let (expr env)
  (let ((binds (cadr expr))
        (body-expr-list (cddr expr)))

    (let (args values)
      (mapcar #'(lambda (a)
                  (let ((arg (car a))
                        (value (cadr a)))
                    (if (not (symbolp arg))
                      (lisp-error-exit expr env))
                    (push arg args)
                    (push (parse-mini-lisp value env) values)))
              binds)
      (setf args (nreverse args))
      (setf values (nreverse values))

      (let* 
        ((func-define (make-func-define :function args body-expr-list))
         (func-closure (make-func-closure func-define (make-new-env env)))
         (func-app (make-app-function func-closure values)))

        (parse-mini-lisp func-app new-env)))))

(defun lisp-fix (expr env)
  (let ((fbinds (cadr expr))
        (body-expr (caddr expr))
        (new-env (make-new-env env)))

    (dolist (fix-func-define fbinds)
      (let ((sym (car fix-func-define)))
        (if (not (symbolp sym))
          (lisp-error-exit expr env))
        (set-key-value sym 
                       (make-func-closure 
                         (copy-tree fix-func-define)
                         new-env))))

    (let* ((fix-main-func-define (make-func-define :function '() body-expr))
           (fix-main-func-closure 
             (make-func-closure fix-main-func-define new-env))
           (fix-main-func-app (make-app-function 
                                fix-main-func-closure 
                                '())))
      (parse-mini-lisp fix-main-func-app new-env))))

(defun lisp-define (expr env)
  (let ((sym-or-func (cadr expr))
        (table (cdr env)))

    (if (listp sym-or-func)

      ; (define (Id Id*) Expr*)
      (let* ((func-name (car sym-or-func))
             (func-args (cdr sym-or-func))
             (func-body (cddr expr))
             (func-define 
               (make-func-define 
                 func-name func-args  func-body))
             (func-closure (make-func-closure func-define (make-new-env env))))

        (set-key-value func-name func-closure env)
        (if (not (symbolp func-name))
          (lisp-error-exit func-name env)))

      ; (define Id Expr)
      (let
        ((sym sym-or-func)
         (value-expr (caddr expr)))

        (if (not (symbolp sym))
          (lisp-error-exit sym env))

        (let ((value (parse-mini-lisp value-expr env)))
          (setf (gethash sym table) value)
          value )))))
