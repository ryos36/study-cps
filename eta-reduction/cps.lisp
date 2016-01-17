;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun lookup-symbol (key env)
  (if (null env) :not-found
    (let ((table (cdr env)))
      (if (null table)
        (lookup-symbol key (car env))
        (multiple-value-bind
          (result exist) (gethash key table)
          (if (null exist)
            (lookup-symbol key (car env))
            result))))))

(defun cps-terminal (expr env)
  (cond
    ((eq expr :#t) :#t)
    ((eq expr :#f) :#f)
    ((null expr) nil)
    ((symbolp expr) (let ((result (lookup-symbol expr env))) 
                      (if (eq result :not-found)
                        expr
                        (cps-terminal result env))))
    (t expr)))

(defun make-new-env (env)
  (cons env (make-hash-table)))

(defun set-key-value (key value env)
  (let ((table (cdr env)))
    (setf (gethash key table) value)))

;----------------------------------------------------------------
(defun make-init-primitive-table ()
  (let ((htable (make-hash-table))
        (primitives `((:+  . t)
                      (:-  . t)
                      (:*  . t)

                      (:>> . t)
                      (:<< . t)

                      (:<  . t)
                      (:>  . t)
                      (:>= . t)
                      (:<= . t)
                      (:=  . t)
                      (:/=  . t)

                      (:heap . t)
                      (:record-set! . t)
                      (:record-ref . t)
                      (:stack . t)
                      (:pop . t))))

      (dolist (src-i (list primitives))
        (mapcar #'(lambda (pair)
                    (let ((key (car pair)) 
                          (value (cdr pair)))
                      (setf (gethash key htable) value)))
                src-i))
      htable))

(defparameter *primitive-table* (make-init-primitive-table))

(defun lookup-primitive (op)
  (gethash op *primitive-table*))
;----------------------------------------------------------------
;(:op (id...) (rv) (cps...))

(defun do-primitive-cps (expr env)
  ;(format t "do-primitive-cps:~a:~a~%" expr (cadddr expr))
  (let ((op (car expr))
        (args (mapcar #'(lambda (x) (cps-terminal x env)) (cadr expr)))
        (rv (cps-terminal (caddr expr) env))
        (next-expr-list (cadddr expr)))
    ;(print `(next-expr-list ,op ,(cadr next-expr-list)))
    (list op args rv
          (mapcar #'(lambda(x) (walk-cps x env)) next-expr-list))))

;----------------------------------------------------------------
(define-condition cps-parse-error (error)
  ((expr :initarg :expr :reader expr) 
   (env :initarg :env :reader env))
  (:report (lambda (condition stream)
             (progn
               (format *error-output* 
                       "CPS Error !!:~s~%" (expr condition))))))

(defun cps-error-exit (expr env)
   (error 'cps-parse-error :expr expr :env env))

;----------------------------------------------------------------
(defun make-env ()
  (let* ((env (make-new-env nil))
         (table (cdr env)))
    (setf (gethash :exit table) '(:exit))
    env))

;----------------------------------------------------------------
;(f (args...) cps)
;  rv found : (func-name . op)
(defun check-eta-reduction (cps-func-expr)
  (let* ((func-name (car cps-func-expr))
         (func-args (cadr cps-func-expr))
         (func-body (caddr cps-func-expr))
         (op (car func-body))
         (op-func-name (cadr func-body))
         (op-args (caddr func-body)))
    (if (and (eq op :app) (equal func-args op-args))
      (cons func-name op-func-name) 
      nil)))

;----------------------------------------------------------------
(defun normalize-reduction-list (re-list)
  (labels ((simple-apply-reduction-list (re-list sym)
           (if (null sym)
             nil
             (progn 
               (dolist (a-b re-list)
                 (if a-b
                   (if (eq (car a-b) sym)
                     (return-from simple-apply-reduction-list 
                                  (simple-apply-reduction-list re-list (cdr a-b))))))
               sym))))

  (mapcar #'(lambda (x) (if x 
                          (cons (car x) 
                                (simple-apply-reduction-list re-list (cdr x)))
                          nil )) re-list)))

;----------------------------------------------------------------
(defun put-reduction-list-into-htable (re-list htable)
  (map nil #'(lambda (re-pair) 
               (let ((src (car re-pair))
                     (dst (cdr re-pair)))
                 (setf (gethash src htable) dst))) re-list))

;----------------------------------------------------------------
;(:fix (binds*) cps)
;
(defun cps-fix (expr env)
  (let* ((fix-op (car expr))
         (binds (cadr expr))
         (next-expr (caddr expr))
         (new-env (make-new-env env))
         (re-binds (mapcar #'(lambda (bind) 
                               (let ((func-name (car bind))
                                     (func-args (cadr bind))
                                     (func-body (caddr bind)))
                                 `(,func-name ,func-args
                                              ,(walk-cps func-body env))))
                           binds))
         (re-list (normalize-reduction-list
                     (mapcar #'check-eta-reduction re-binds)))
         new-binds
         (table (cdr new-env)))

    (put-reduction-list-into-htable re-list table) 

    (let ((new-binds
            (remove-if #'null
                       (map 'list #'(lambda(bind re)
                                      (if re nil (cps-define bind new-env)))
                            binds re-list)))
          (new-cps (walk-cps next-expr new-env)))
      (if new-binds
        (list fix-op new-binds new-cps)
        new-cps))))

;----------------------------------------------------------------
;(app f (id...))
(defun cps-app (expr env)
  (let ((call-func-name (cps-terminal (cadr expr) env))
        (args 
          (mapcar #'(lambda (x) (cps-terminal x env))
                      (caddr expr))))
    ;(print `(cps-app ,call-func-name ,(caddr expr) ,args ,(cps-terminal (car args) env)))
    (copy-tree `(:app ,call-func-name ,args))))


;----------------------------------------------------------------
;(:exit (id) () ())
(defun cps-exit (expr env)
  (let ((arg (cadr expr)))
    (if (not (symbolp (car arg))) (cps-error-exit expr env))
    (copy-tree `(:exit ,(cps-terminal arg env) () ()))))

;----------------------------------------------------------------
#|
(defun n-reduction0 (expr func-name op-call-func target-expr)
  (let ((new-expr (subst op-call-func func-name target-expr)))
    new-expr))


(defun n-reduction (expr &optional rv)
  (if (eq (car expr) :fix)
    (let* ((binds (cadr expr))
           (checked-binds
             (mapcar #'(lambda (abind)
               (let*((func-name (car abind))
                     (func-args (cadr abind))
                     (bind-expr (caddr abind))
                     (op (car bind-expr))
                     (op-call-func(cadr bind-expr))
                     (op-args (caddr bind-expr)))
                 (when (and (eq op :app)
                            (equal func-args op-args))
                   (format t "You can change ~a -> ~a~%" func-name op-call-func)
                   (cons func-name op-call-func)))) binds))
           a-to-b-list
           )
      (format t "change expr : ~s~%" checked-binds)
      (setf a-to-b-list
              (mapcar #'(lambda (x) (when x 
                                      (setf (cdr x) (a-to-b checked-binds (cdr x)))
                                      x )) checked-binds))
      (format t "a-to-b: ~s~%" a-to-b-list)
      (mapcar #'(lambda (a-bind) 
                  (let ((func-name (car a-bind))
                        (func-remain (cdr a-bind)))
                    (setf (cdr a-bind) (do-a-to-b a-to-b-list func-remain))))
              binds)
      (format t "done : ~s~%" binds)
      nil))
  (let ((new-expr (cadddr expr)))
    (format t "new-expr:~a~%" new-expr )
    (if new-expr
      (map nil #'(lambda (each-expr) (n-reduction each-expr)) new-expr))))
|#

;----------------------------------------------------------------
;(f (args...) cps)
(defun check-cps-define (expr)
  (let ((func-name (car expr))
        (func-args (cadr expr))
        (func-body (caddr expr)))
    (and (symbolp func-name)
         (listp func-body))))

;(func-name (args) cps)
(defun cps-define (expr env)
  (let ((func-name (car expr))
        (func-args (cadr expr))
        (func-body (caddr expr)))
    (list func-name
          (mapcar #'(lambda (x) (cps-terminal x env)) func-args)
          (walk-cps func-body env))))
        
;----------------------------------------------------------------
(defparameter *debug-mode* nil)
(defparameter *cps-stack* nil)
(defparameter *env* (make-new-env nil))
(defparameter *cps-exit-output* nil)

;----------------------------------------------------------------
(defun walk-cps (expr env)
  (if *debug-mode*
    (format t "expr:~s~%" expr))

  (if (terminal-p expr)
    (cps-terminal expr env)
    (let ((op (car expr)))
      (case op
        (:fix (cps-fix expr env))
        (:fixs (cps-fix expr env))
        (:fixh (cps-fix expr env))
        (:app (cps-app expr env))
        ;(:let )
        (:exit (cps-exit expr env))
        (otherwise (let ((primitive-cps (lookup-primitive op)))
                     ;(format t "primitive-cps:~a ~s~%" op primitive-cps)
                     (if (null primitive-cps)
                       (if (check-cps-define expr)
                         (cps-define expr env)
                         (cps-error-exit expr env))
                       (do-primitive-cps expr env))))))))

