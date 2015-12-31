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
  (cons env (make-hash-table)))

(defun set-key-value (key value env)
  (let ((table (cdr env)))
    (setf (gethash key table) value)))
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
  (let* ((env (make-new-env nil))
         (table (cdr env)))
    (setf (gethash :exit table) '(:exit))
    env))


;----------------------------------------------------------------
(defun make-cps-closure (bind new-env)
  (cons bind new-env))

;----------------------------------------------------------------
(defun cps-fix (expr env)
  (let ((binds (cadr expr))
        (next-expr (caddr expr))
        (new-env (make-new-env env)))

    (map nil #'(lambda (func-define)
                 (let ((func-name (car func-define))
                       (cps-closure (make-cps-closure func-define new-env)))

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
  (let* ((func-name (cadr expr))
         (args (caddr expr))
         (cps-closure (lookup-symbol func-name env))
         (func-define (car cps-closure))
         (func-env (cdr cps-closure))
         (arg-syms (cadr func-define))
         (next-expr (caddr func-define)))

    (map nil #'(lambda (key arg) 
                 (let ((value (parse-expr-terminal arg env)))
                   (set-key-value key value func-env)))
         arg-syms args)

    (parse-cps next-expr func-env)))


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
    (format t "expr:~s~%" expr))

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

