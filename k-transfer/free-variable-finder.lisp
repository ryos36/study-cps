;----------------------------------------------------------------
(load "cps-parser.lisp")

;----------------------------------------------------------------
(defclass free-variable-finder (cps-parser)
  ())

;----------------------------------------------------------------
(defmethod make-new-env ((parser free-variable-finder) env)
  (cons `(()) env))

;----------------------------------------------------------------
(defmethod find-variable ((parser free-variable-finder) expr env)
  (if (null env)
    nil
    (let* ((top-env (car env))
           (key-value (assoc expr top-env)))
      (if key-value key-value
        (find-variable parser expr (cdr env))))))

;----------------------------------------------------------------
(defmethod set-variable ((parser free-variable-finder) key value env)
  (let ((top-env (car env)))
    (setf (car env) (cons `(,expr . nil) top-env))))

;----------------------------------------------------------------
(def-cps-func cps-terminal ((parser free-variable-finder) expr env)
  (let ((variable-key-value (find-variable parser expr env)))
    (if (null variable-key-value)
      (progn
        (set-variable parser expr nil env)
        `(,expr :free-variable))
      expr)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser free-variable-finder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (let ((variable-key-value (find-variable parser expr env))
      (if (null variable-key-value)
        (set-variable parser expr nil env)))

      (mapcar #'(lambda(arg) 
                  (set-variable parser expr t env)) args)
      (let ((new-next-cps (cps-parser parser next-cps bind env)))
        `(,(if variable-key-value expr (expr :free-variable))
           ,args ,new-next-cps)))))
