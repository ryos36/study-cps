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
        (body-expr-list (cddr expr))
        (htable (make-hash-table)))
    (dolist (arg-pair binds)
      (let ((sym (car arg-pair))
            (value-expr (cadr arg-pair)))
        (if (not (symbolp sym))
          (lisp-error-exit sym env))
        (setf (gethash sym htable) (parse-mini-lisp value-expr env))))
    (let ((new-env (list env htable)))
      (dolist (body-expr body-expr-list)
        (parse-mini-lisp body-expr new-env)))))

(defun lisp-fix (expr env)
  (let ((fbinds (cadr expr))
        (body-expr (caddr expr))
        (htable (make-hash-table)))
    (dolist (func-pair fbinds)
      (let ((sym (car func-pair))
            (fix-expr (copy-tree (cdr func-pair))))
        (if (not (symbolp sym))
          (lisp-error-exit sym env))
        (setf (gethash sym htable) fix-expr)))
    (let ((new-env (list env htable)))
      (parse-mini-lisp body-expr new-env))))

(defun lisp-define (expr env)
  (let ((sym-or-func (cadr expr))
        (table (cadr env)))
    (if (listp sym-or-func)

      ; (define (Id Id*) Expr*)
      (let ((func-name (car sym-or-func))
            (func-args (cdr sym-or-func))
            (func-body-expr (cddr expr)))

        (if (not (symbolp func-name))
          (lisp-error-exit func-name env))

        (setf (gethash func-name table) 
              (cons (copy-tree func-args) (copy-tree func-body-expr))))

      ; (define Id Expr)
      (let
        ((sym sym-or-func)
         (value-expr (caddr expr)))

        (if (not (symbolp sym))
          (lisp-error-exit sym env))

        (let ((value (parse-mini-lisp value-expr env)))
          (setf (gethash sym table) value)
          value )))))
