;----------------------------------------------------------------
(load "cps-parser.lisp")

;----------------------------------------------------------------
(defclass free-variable-finder (cps-parser)
  ())

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
    (setf (car env) (cons `(,key . ,value) top-env))))

;----------------------------------------------------------------
(def-cps-func cps-symbol ((parser free-variable-finder) expr env)
  (let ((variable-key-value (find-variable parser expr env)))
    (if (null variable-key-value)
      (progn
        (set-variable parser expr nil env)
        `(:free-variable ,expr))
      expr)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser free-variable-finder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (set-variable parser func-name t env)
    (mapcar #'(lambda(arg) 
                (set-variable parser arg t env)) args)

    (let ((new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser free-variable-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (mapcar #'(lambda (r) (set-variable parser r t env)) result)

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

