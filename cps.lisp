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
      (progn
        (format t "parse-expr-terminal ~a~%" expr)
      expr)))

(defun make-new-env (env)
  (cons env (make-hash-table)))

(defun set-key-value (key value env)
  (let ((table (cdr env)))
    (setf (gethash key table) value)))
;----------------------------------------------------------------
; primitive
;(load "cps-primitive.lisp")
(defun cps-+ (expr env)
  (let ((args (cadr expr))
        (rv (caddr expr))
        (next-expr (cadddr expr)))

    (format t "~a~%" args)

    (let ((arg0 (parse-expr-terminal (car args) env))
          (arg1 (parse-expr-terminal (cadr args) env))
          (new-env (make-new-env env)))
      (set-key-value rv (+ arg0 arg1) new-env)
      (parse-cps next-expr new-env))))

(defun cps-record-ref (expr)
  )

;----------------------------------------------------------------
(defun make-init-primitive-table ()
  (let ((htable (make-hash-table))
        (primitives `((:+  . ,#'cps-+)
                      #|
                      (:-  . ,#'cps--)
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
      (format t "cps-fix:~a = ~a~%" func-name bind-func-body)
      (format t "   next-expr:~a~%" next-expr)
      (format t "   my-func:~a~%" (lookup-symbol func-name new-env))
      (format t "   FIX my-func:~a~%" (lookup-symbol 'f new-env))
      (parse-cps next-expr new-env))))

;----------------------------------------------------------------
(defun cps-app (expr env)
  (format t "   APP my-func:~a~%" (lookup-symbol 'f env))
  (let ((func-name (cadr expr))
        (args (caddr expr))
        (new-env (make-new-env env)))
    (let* ((bind-func-body (lookup-symbol func-name env))
           (arg-syms (cadr bind-func-body))
           (next-expr (caddr bind-func-body)))
      (map nil #'(lambda (key arg) 
                   (format t "set-key-value ~a ~a ~a~%" key arg
                           (parse-expr-terminal arg env)
                           )
                   (let ((value (parse-expr-terminal arg env)))
                     (set-key-value key value new-env))) 
           arg-syms args)
      (format t "new-env ~a a:~a b:~a~%"
              arg-syms
              (lookup-symbol 'a new-env)
              (lookup-symbol 'b new-env))

      (parse-cps next-expr new-env))))


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
      (:exit t)
      (otherwise (let ((primitive-cps (lookup-primitive op)))
                   ;(format t "primitive-cps:~a ~s~%" op primitive-cps)
                   (funcall primitive-cps expr env))))))

