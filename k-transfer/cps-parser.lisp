;----------------------------------------------------------------
(in-package :sinby.cps.parser)

;----------------------------------------------------------------
(defclass cps-parser () 
  ((sym-no :initform 0 :initarg :sym-no)
   (sym-name :initform "sym" :initarg :sym-name)))

;----------------------------------------------------------------
(defgeneric make-new-env (parser env &optional new-env-item )
            (:documentation "Make a new environment."))

;----------------------------------------------------------------
(defmethod cps-gensym ((parser cps-parser) &optional is-label)
  (let ((rv (intern (format nil "~a~a~a" (if is-label ":" "")
                            (slot-value parser 'sym-name)
                            (slot-value parser 'sym-no)))))
    (incf (slot-value parser 'sym-no))
    rv))

;----------------------------------------------------------------
(defmethod make-new-env ((parser cps-parser) env &optional (new-env-item `(())))
  (cons new-env-item env))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or 
    (eq :#t expr)
    (eq :#f expr)
    (eq :unspecified expr)
    (null expr)
    (symbolp expr)
    (numberp expr)))

;----------------------------------------------------------------
(defun cps-symbolp (expr)
  (not (or 
         (eq :#t expr)
         (eq :#f expr)
         (eq :unspecified expr)
         (null expr)
         (numberp expr)
         (and (listp expr) (eq (car expr) :label)))))

;----------------------------------------------------------------
(defmethod cps-primitive-p ((parser cps-parser) op)
  (case op 
    (:#t nil)
    (:#f nil)

    (:+  t)
    (:-  t)
    (:*  t)

    (:>> t)
    (:<< t)

    (:<  t)
    (:>  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)

    (:heap t)
    (:record-set! t)
    (:record-ref t)
    (:record-offs t)
    (:stack t)
    (:pop t)

    (otherwise nil)))

;----------------------------------------------------------------
(defun compare-primitivep (sym)
  (case sym
    (:>  t)
    (:<  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)
    (otherwise nil)))

;----------------------------------------------------------------
(defmacro def-cps-func (func-name &rest func-define)
  (let* ((args (car func-define))
           (parser-pair (car args))
             (parser (car parser-pair))
           (expr (cadr args))
           (env (caddr args))

         (func-body (cdr func-define)))

    `(defmethod ,func-name ,args ,@func-body)))

;----------------------------------------------------------------
(def-cps-func cps-terminal ((parser cps-parser) expr env)
  (cond
    ((eq :#t expr) :#t)
    ((eq :#f expr) :#f)
    ((eq :unspecified expr) :unspecified)
    ((null expr) nil)
    ((symbolp expr) (cps-symbol parser expr env))
    (t expr)))

;----------------------------------------------------------------
(def-cps-func cps-symbol ((parser cps-parser) expr env)
  expr)

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-parser) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))
    (let ((new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((parser cps-parser) binds env)
  (mapcar #'(lambda (bind) (cps-bind parser bind env)) binds))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-parser) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr)))
        
    (let ((new-binds (cps-binds parser binds env))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,fix-op ,new-binds ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-fixh ((parser cps-parser) expr env)
  (cps-fix parser expr env))

;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser cps-parser) expr env)
  (cps-fix parser expr env))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-parser) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))
        
    (let ((new-func-name (cps-symbol parser func-name env))
          (new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args)))
      `(:APP ,new-func-name ,new-args))))
;----------------------------------------------------------------
(def-cps-func cps-exit ((parser cps-parser) expr env)
  (let ((arg0 (caadr expr)))
    (let ((new-arg0 (cps-terminal parser arg0 env)))
      `(:EXIT (,new-arg0) () ()))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-parser) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

;----------------------------------------------------------------
(defmethod cps-error-exit ((parser cps-parser) expr env)
   (error 'parse-error :expr expr :env env))

;----------------------------------------------------------------
(defmethod cps-parse ((parser cps-parser) expr env)
  (if (terminal-p expr)
    (cps-terminal parser expr env)
    (let ((op (car expr)))
      (case op
        (:fixs (cps-fixs parser expr env))
        (:fixh (cps-fixh parser expr env))
        (:app (cps-app parser expr env))
        (:neq (cps-neq parser expr env))
        (:exit (cps-exit parser expr env))
        (otherwise (if (cps-primitive-p parser op)
                     (cps-primitive parser expr env)
                     (cps-error-exit parser expr env)))))))


