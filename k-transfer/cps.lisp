;----------------------------------------------------------------
(load "utils.lisp")
(load "make-cxr-route.lisp")

;----------------------------------------------------------------
(defclass cps-parser () 
  ())

;----------------------------------------------------------------
(defgeneric make-new-env ((parser cps-parser) env))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or 
    (eq :#t expr)
    (eq :#f expr)
    (null expr)
    (symbolp expr)
    (numberp expr)))

;----------------------------------------------------------------
(defmethod cps-terminal ((parser cps-parser) expr env)
  (let* ((new-env (make-env-cps-terminal parser expr env))
         (walk-result expr)
         (new-expr (make-expr-cps-terminal parser expr env new-env walk-result)))
    new-expr))
  
;----------------------------------------------------------------
(defun make-prefix-func-name (prefix func-name)
        (intern (format nil "~a-~a" prefix func-name)))

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

(defmacro def-env-func (func-name &rest func-define)
  (let* ((make-env-func (make-prefix-func-name 'make-env func-name))
        (args (car func-define))
          (parser-pair (car args))
            (parser (car parser-pair))
          (expr (cadr args))
          (env (caddr args))
        (func-body (cdr func-define)))

    `(defmethod ,make-env-func ,args ,@func-body)))

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
(def-env-func cps-fix ((parser cps-parser) expr env)
  (make-new-env env))

(def-cps-func cps-fix ((parser cps-parser) expr env)
  (let ((binds (cadr expr))
        (next-cps (caddr expr))
        (new-parser (make-instance 'find-free-variable))
        (new-env (make-new-env (new-parser) nil)))
        
    (let ((free-variable (cps-parser new-parser expr new-env))
    (cons (mapcar #'(lambda (bind) (cps-parse parser bind new-env)) binds)
          (cps-parse parser next-cps env))))

(def-expr-func cps-fix ((parser cps-parser) expr new-env walk-result)
   (let ((fix-op (car expr))
         (new-binds (car walk-result))
         (new-next-expr (cdr walk-result)))

     `(,fix-op ,new-binds ,new-next-expr)))

;----------------------------------------------------------------
(defun cps-id (expr env)
  )

;----------------------------------------------------------------
(defun cps-neq (expr env)
  )
;----------------------------------------------------------------
(defun cps-app (expr env)
  )

;----------------------------------------------------------------
;(:exit (r) () ())
(defun cps-exit (expr env)
  )

;----------------------------------------------------------------
(defparameter *debug-mode* nil)
(defparameter *cps-stack* nil)
(defparameter *env* (make-new-env nil))
(defparameter *cps-exit-output* nil)

;----------------------------------------------------------------

#|
(defgeneric cps-parse ((parser cps-parser) expr env)
            "cps-parse")
|#

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
