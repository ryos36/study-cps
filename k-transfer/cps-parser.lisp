;----------------------------------------------------------------
(defclass cps-parser () 
  ())

;----------------------------------------------------------------
(defgeneric make-new-env (parser env)
            (:documentation "Make a new environment."))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or 
    (eq :#t expr)
    (eq :#f expr)
    (null expr)
    (symbolp expr)
    (numberp expr)))

;----------------------------------------------------------------
(defun make-prefix-func-name (prefix func-name)
        (intern (format nil "~a-~a" prefix func-name)))

;----------------------------------------------------------------
(defmacro def-cps-func (func-name &rest func-define)
  (let* ((make-env-func (make-prefix-func-name 'make-env func-name))
         (make-expr-func (make-prefix-func-name 'make-expr func-name))
         (args (car func-define))
         (parser-pair (car args))
         (parser (car parser-pair))
         (expr (cadr args))
         (env (caddr args))

         (func-body (cdr func-define))
         (new-env (gensym))
         (walk-result (gensym))
         (new-expr (gensym)))

    `(defmethod ,func-name ,args
       (let* ((,new-env (,make-env-func ,parser ,expr ,env))
              (,walk-result (let ((,env ,new-env)) ,@func-body))
              (,new-expr (,make-expr-func ,parser ,expr ,env ,new-env ,walk-result)))
         ,new-expr))))

;----------------------------------------------------------------
(defmacro def-env-func (func-name &rest func-define)
  (let* ((make-env-func (make-prefix-func-name 'make-env func-name))
        (args (car func-define))
          (parser-pair (car args))
            (parser (car parser-pair))
          (expr (cadr args))
          (env (caddr args))
        (func-body (cdr func-define)))

    `(defmethod ,make-env-func ,args ,@func-body)))

;----------------------------------------------------------------
(defmacro def-expr-func (func-name &rest func-define)
  (let* ((make-expr-func (make-prefix-func-name 'make-expr func-name))
        (args (car func-define))
          (parser-pair (car args))
            (parser (car parser-pair))
          (expr (cadr args))
          (env (caddr args))
          (walk-result (cadddr args))
        (func-body (cdr func-define)))

    `(defmethod ,make-expr ,args ,@func-body)))

;----------------------------------------------------------------
(def-env-func cps-terminal ((parser cps-parser) expr env) env)
(def-env-func cps-fixs ((parser cps-parser) expr env) env)
(def-env-func cps-fixh ((parser cps-parser) expr env) env)
(def-env-func cps-app ((parser cps-parser) expr env) env)
(def-env-func cps-exit ((parser cps-parser) expr env) env)
(def-env-func cps-primitive ((parser cps-parser) expr env) env)

;----------------------------------------------------------------
(def-cps-func cps-terminal ((parser cps-parser) expr env)
  expr)

;----------------------------------------------------------------
(def-expr-func cps-terminal ((parser cps-parser) expr env walk-result) walk-result)

;----------------------------------------------------------------

(def-cps-func cps-bind ((parser cps-parser) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (cps-parser parser next-cps bind env)
    expr))
;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser cps-parser) expr env)
  (let ((binds (cadr expr))
        (next-cps (caddr expr)))
        
    (cons (mapcar #'(lambda (bind) (cps-bind parser bind env)) binds)
          (cps-parse parser next-cps env))))
;----------------------------------------------------------------
(def-cps-func cps-fixh ((parser cps-parser) expr env)
  (let ((binds (cadr expr))
        (next-cps (caddr expr)))
        
    (cons (mapcar #'(lambda (bind) (cps-bind parser bind env)) binds)
          (cps-parse parser next-cps env))))
;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-parser) expr env)
  (let ((op (cadr expr))
        (args (caddr expr)))
        
    (cons (mapcar #'(lambda (bind) (cps-parse parser op env)) binds)
          (cps-parse parser args env))))
(def-expr-func cps-fixs ((parser cps-parser) expr env walk-result) expr)
(def-expr-func cps-fixh ((parser cps-parser) expr env walk-result) expr)
(def-expr-func cps-app ((parser cps-parser) expr env walk-result) expr)
(def-expr-func cps-exit ((parser cps-parser) expr env walk-result) expr)
(def-expr-func cps-primitive ((parser cps-parser) expr env walk-result) expr)
;----------------------------------------------------------------

(defmethod cps-parse ((parser cps-parser) expr env)

  (if (terminal-p expr)
    (cps-terminal parser expr context)
    (let ((op (car expr)))
      (case op
        (:fixs (cps-fixs parser expr env))
        (:fixh (cps-fixh parser expr env))
        (:app (cps-app parser expr env))
        (:neq (cps-neq parser expr env))
        (:exit (cps-exit parser expr env))
        (otherwise (if (primitive? (parser op))
                     (cps-primitive parser expr env)
                     (cps-otherwise parser expr env)))))))
