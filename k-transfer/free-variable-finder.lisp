;----------------------------------------------------------------
(load "cps-parser.lisp")

;----------------------------------------------------------------
(defclass free-variable-finder (cps-parser)
  ())

;----------------------------------------------------------------
(defmethod find-variable ((parser free-variable-finder) expr env)
  (if (null env)
    nil
    (let ((top-env (car env)))
      (if (atom top-env)
        nil
        (let ((key-value (assoc expr top-env)))
          (if key-value key-value
            (find-variable parser expr (cdr env))))))))

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
        (next-cps (caddr expr))
        (new-env (make-new-env parser env)))

    ;(print `(cps-bind ,func-name))
    (mapc #'(lambda(arg) 
                (set-variable parser arg t new-env)) args)

    (let ((new-next-cps (cps-parse parser next-cps new-env)))
       (mapc #'(lambda(arg) 
                 (set-variable parser arg nil env))
             (nreverse (filter-free-variables (car new-env))))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((parser free-variable-finder) binds env)
  (let ((func-names (mapcar #'(lambda (x) (car x)) binds)))
    (mapc #'(lambda(arg) 
                (set-variable parser arg t env)) func-names)
    (mapcar #'(lambda (bind) (cps-bind parser bind env)) binds)))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser free-variable-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (mapc #'(lambda (r) (set-variable parser r t env)) result)

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun filter-free-variables (all-variables)
  (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) all-variables)))

;----------------------------------------------------------------
(defun get-free-variables (finder cps-func expr)
  (let ((finder-env (make-new-env finder '())))
    (funcall cps-func finder expr finder-env)
    (let* ((all-variables (car finder-env))
           (free-variables (filter-free-variables all-variables)))
      free-variables)))
