;----------------------------------------------------------------
(defun l ()
  (load "cps.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun lookup-symbol (key env)
  (if (null env) nil
    (let ((table (cdr env)))
      (if (null table)
        (lookup-symbol key (car env))
        (let ((result (gethash key table)))
          (if (null result)
            (lookup-symbol key (car env))
            result))))))

(defun parse-expr-terminal (expr env)
  (if (symbolp expr)
      (let ((result (lookup-symbol expr env)))
                   (if (null result) (cps-error-exit expr env)
                     result))
      expr))

(defun make-new-env (env)
  (cons env (make-hash-table)))

(defun set-key-value (key value env)
  (let ((table (cdr env)))
    (setf (gethash key table) value)))
;----------------------------------------------------------------
; primitive
;(load "cps-primitive.lisp")
(defmacro cps-two (expr env &rest body)
  `(let ((args (cadr ,expr))
         (rv (caaddr ,expr))
         (next-expr (cadddr ,expr)))

     (let ((arg0 (parse-expr-terminal (car args) ,env))
           (arg1 (parse-expr-terminal (cadr args) ,env))
           (new-env (make-new-env ,env)))
       (set-key-value rv ,@body new-env)
       (parse-cps next-expr new-env))))

(defun cps-+ (expr env)
  (let ((args (cadr expr))
        (rv (caaddr expr))
        (next-expr (cadddr expr)))

    (let ((arg0 (parse-expr-terminal (car args) env))
          (arg1 (parse-expr-terminal (cadr args) env))
          (new-env (make-new-env env)))
      (set-key-value rv (+ arg0 arg1) new-env)
      (parse-cps next-expr new-env))))

(defun cps-- (expr env)
  (cps-two expr env (- arg0 arg1)))

(defun cps-record-ref (expr)
  )

;----------------------------------------------------------------
(defun make-init-primitive-table ()
  (let ((htable (make-hash-table))
        (primitives `((:+  . ,#'cps-+)
                      (:-  . ,#'cps--)
                      #|
                      (:>> . ,#'cps->>)
                      (:<< . ,#'cps-<<)
                      (:<  . ,#'cps-<)
                      (:>  . ,#'cps->)
                      (:>= . ,#'cps->=)
                      (:<= . ,#'cps-<=)
                      (:=  . ,#'cps-=)
                      (:heap . ,#'cps-heap)
                      (:stack . ,#'cps-stack)
                      (:pop . ,#'cps-pop)
                      (:record-set! . ,#'cps-record-set!)
                      |#
                      (:record-ref . ,#'cps-record-ref))))

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
                       "CPS Error !!:<~s>~%" (expr condition))))))

(defun cps-error-exit (expr env)
   (error 'cps-parse-error :expr expr :env env))

;----------------------------------------------------------------
(defun make-env ()
  (let* ((env (make-new-env nil))
         (table (cdr env)))
    (setf (gethash :exit table) '(:exit))
    env))

;----------------------------------------------------------------
(defun cps-fix (expr env)
  (let ((bind-func-body (cadr expr))
        (next-expr (caddr expr))
        (new-env (make-new-env env)))
    (let ((func-name (car bind-func-body)))
      (set-key-value func-name bind-func-body new-env)
      (parse-cps next-expr new-env))))

;----------------------------------------------------------------
(defun cps-app (expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))
        (new-env (make-new-env env)))
    (let* ((bind-func-body (lookup-symbol func-name env))
           (arg-syms (cadr bind-func-body))
           (next-expr (caddr bind-func-body)))
      (map nil #'(lambda (key arg) 
                   (let ((value (parse-expr-terminal arg env)))
                     (set-key-value key value new-env))) 
           arg-syms args)

      (parse-cps next-expr new-env))))


;----------------------------------------------------------------
(defun cps-exit (expr env)
  (let ((result (parse-expr-terminal (cadr expr) env)))
  (format t "cps-exit:~a~%" result)
  result))
        
;----------------------------------------------------------------
(defparameter *debug-mode* nil)

;----------------------------------------------------------------
(defun parse-cps (expr env &optional)
  (if *debug-mode*
    (format t "expr:~s~%" expr))

  (let ((op (car expr)))
    (case op
      (:fix (cps-fix expr env))
      (:app (cps-app expr env))
      (:exit (cps-exit expr env))
      (otherwise (let ((primitive-cps (lookup-primitive op)))
                   ;(format t "primitive-cps:~a ~s~%" op primitive-cps)
                   (if (null primitive-cps)
                     (cps-error-exit expr env))
                   (funcall primitive-cps expr env))))))

