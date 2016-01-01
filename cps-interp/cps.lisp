;----------------------------------------------------------------
(defun l ()
  (load "cps.lisp"))

;----------------------------------------------------------------
(defun caadddr (tree)
         (car (cadddr tree)))

(defun cadadddr (tree)
        (cadr (cadddr tree)))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun env-length (env &optional rv)
  (length env))

(defun lookup-symbol (key env)
  (if (null env) 
    :not-found
    (let ((table (car env)))
      (if (null table) 
        :not-found
        (let ((key-value (assoc keY table)))
          (if (null key-value) 
            (lookup-symbol key (cdr env))
            (cadr key-value)))))))

(defun parse-expr-terminal (expr env)
  (cond 
    ((eq :#t expr) :#t)
    ((eq :#f expr) :#f)
    ((null expr) nil)
    ((symbolp expr)
     (let ((result (lookup-symbol expr env))) 
       (if (eq result :not-found)
         (cps-error-exit expr env)
           result)))
    (t expr)))

(defun make-new-env (env)
  (cons `(()) env))

(defun set-key-value (key value env)
  (let ((table (car env)))
    (setf (car env) (push `(,key ,value) table))))
;----------------------------------------------------------------
; primitive
(load "primitive.lisp")

;----------------------------------------------------------------
(defun make-init-primitive-table ()
  (let ((htable (make-hash-table))
        (primitives `((:+  . ,#'cps-+)
                      (:-  . ,#'cps--)
                      (:*  . ,#'cps-*)

                      (:>> . ,#'cps->>)
                      (:<< . ,#'cps-<<)
                      (:<  . ,#'cps-<)
                      (:>  . ,#'cps->)
                      (:>= . ,#'cps->=)
                      (:<= . ,#'cps-<=)
                      (:=  . ,#'cps-=)
                      (:/= . ,#'cps-/=)

                      (:heap . ,#'cps-heap)
                      (:record-set! . ,#'cps-record-set!)
                      (:record-ref . ,#'cps-record-ref)
                      (:stack . ,#'cps-stack)
                      (:pop . ,#'cps-pop))))

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
  (let* ((env (make-new-env nil)))
    env))

;----------------------------------------------------------------
(defun save-cps-closure (bind new-env)
  `(:closure ,bind ,new-env))

(defun make-cps-closure (func-name env)
  (let ((saved-cps-closure (parse-expr-terminal func-name env)))
    (if (eq :closure (car saved-cps-closure))
      (let ((bind (cadr saved-cps-closure))
            (closure-env (caddr saved-cps-closure)))
        (let ((copy-env (mapcar #'(lambda (an-env)
                               (copy-list an-env)) closure-env)))

          (print `(copy-env ,copy-env))
          `(:closure ,bind ,copy-env)))
      (cps-error-exit `(,func-name ,saved-cps-closure env)))))

;----------------------------------------------------------------
(defun cps-fix (expr env)
  (let ((binds (cadr expr))
        (next-expr (caddr expr))
        (new-env (make-new-env env)))

    (map nil #'(lambda (func-define)
                 (let ((func-name (car func-define))
                       (cps-closure (save-cps-closure func-define new-env)))

                   (print `(:cps-fix ,func-name ,func-define ,(env-length env 0)))
                   (set-key-value func-name cps-closure new-env)))
         binds)
    (parse-cps next-expr new-env)))

;----------------------------------------------------------------
(defun cps-id (expr env)
  (let* ((arg0 (parse-expr-terminal (caadr expr) env))
         (result (caaddr expr))
         (next-expr (caadddr expr))
         (new-env (make-new-env env)))

    (set-key-value result arg0 new-env)
    (parse-cps next-expr new-env)))

;----------------------------------------------------------------
(defun cps-neq (expr env)
  (let* ((arg0 (parse-expr-terminal (caadr expr) env))
         (arg1 (parse-expr-terminal (cadadr expr) env))

         (true-expr (caadddr expr))
         (false-expr (cadadddr expr)))

    (parse-cps (if (not (eq arg0 arg1)) true-expr false-expr) env)))
;----------------------------------------------------------------
(defun cps-app (expr env)
 ;(print `(:cps-app-first ,(parse-expr-terminal (cadr expr) env) search-SYM5 ,(lookup-symbol 'SYM5 env) ,(env-length env 0)))
  (let* ((func-name (parse-expr-terminal (cadr expr) env))
         (args (caddr expr))
         (x (print 'app))
         (cps-closure (make-cps-closure func-name env))
         (func-define (cadr cps-closure))
         (func-env (caddr cps-closure))
         (arg-syms (cadr func-define))
         (next-expr (caddr func-define))
         (new-env (make-new-env func-env))
         v-args
         )

    (map nil #'(lambda (key arg) 
                 (let ((value (parse-expr-terminal arg env)))
                   (push `(,key ,arg ,(if (listp value) 'LIST value)) v-args)
                   (set-key-value key value new-env)))
         arg-syms args)

    (print `(:do-app ,func-name ,v-args, (env-length new-env 0)))
    ;(print `(:do-app-app ,func-name ,next-expr))

    (if (eq func-name 'sym2)
      (let* ((sym5-closure (parse-expr-terminal 'sym2 new-env))
             (sym5-bind (cadr sym5-closure))
             (sym5-name (car sym5-bind))
             (sym5-args (cadr sym5-bind))
             (sym5-body (caddr sym5-bind)))
        (print `(SYM2 IS ,sym5-name))
        (print `(ARG IS ,sym5-args))
        (print `(FUNC IS ,sym5-body))
        ;(exit 0)
        ))

    (parse-cps next-expr new-env)))

;----------------------------------------------------------------
;(:exit (r) () ())
(defun cps-exit (expr env)
  (let ((result (parse-expr-terminal (caadr expr) env)))
    (format *cps-exit-output* "cps-exit:~a~%" result)
    result))
        
;----------------------------------------------------------------
(defparameter *debug-mode* nil)
(defparameter *cps-stack* nil)
(defparameter *env* (make-new-env nil))
(defparameter *cps-exit-output* nil)

;----------------------------------------------------------------
(defun parse-cps (expr env)
  (if *debug-mode*
    (format t "expr:~a ~s~%" (env-length env 0) expr))

  (let ((op (car expr)))
    (case op
      (:fix (cps-fix expr env))
      (:fixs (cps-fix expr env))
      (:fixh (cps-fix expr env))
      (:app (cps-app expr env))
      (:id (cps-id expr env))
      (:neq (cps-neq expr env))
      (:exit (cps-exit expr env))
      (otherwise (let ((primitive-cps (lookup-primitive op)))
                   ;(format t "primitive-cps:~a ~s~%" op primitive-cps)
                   (if (null primitive-cps)
                     (cps-error-exit expr env))
                   (funcall primitive-cps expr env))))))

