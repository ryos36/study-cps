;----------------------------------------------------------------
(defun l ()
  (load "mini-scheme.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (null expr)
      (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun lookup-symbol (key env)
  (if (null env) nil
    (let ((table (cadr env)))
      (if (null table)
        (lookup-symbol key (car env))
        (let ((result (gethash key table)))
          (if (null result)
            (lookup-symbol key (car env))
            result))))))

(defun parse-expr-terminal (expr env)
  (if (symbolp expr)
    (case expr
      (:#t :#t)   ; simulate scheme #t
      (:#f :#f) ; simulate scheme #f
      (otherwise (let ((result (lookup-symbol expr env)))
                   (if (null result) (error-exit expr env)
                     result))))
    expr))

;----------------------------------------------------------------
(defun lookup-parser (op table-key env)
  (if (null env) nil
    (let ((table (gethash table-key (cadr env))))
      (if (null table)
        (lookup-parser op table-key (car env))
        (gethash op table)))))

;----------------------------------------------------------------
(define-condition scheme-parse-error (error)
  ((expr :initarg :expr :reader expr) 
   (env :initarg :env :reader env))
  (:report (lambda (condition stream)
             (progn
               (format *error-output* 
                       "Error !!:<~s>~%" (expr condition))))))

(defun error-exit (expr env)
   (error 'scheme-parse-error :expr expr :env env))

;----------------------------------------------------------------
; primitive
(load "primitive.lisp")

;----------------------------------------------------------------
; essential
(load "essential.lisp")

;----------------------------------------------------------------
(defun parser-app (expr env)
  (let (result
         (func-define (car expr))
         (values (cdr expr)))
    (if (symbolp func-define)
      (setf func-define (lookup-symbol func-define env)))
    (if (null func-define)
      (error-exit expr env))
    (let ((args (car func-define))
          (body-expr-list (cdr func-define))
          (htable (make-hash-table))
          value)
      (dolist (arg args)
        (setf value (car values))
        (setf values (cdr values))
        (if (not (symbolp arg))
          (error-exit sym env))
        (setf (gethash arg htable) (parse-mini-scheme value env)))

      (let ((new-env (list env htable)) r)
        (dolist (body-expr body-expr-list)
          (setf result
                (parse-mini-scheme body-expr new-env))
          )))
    result))

;----------------------------------------------------------------
(defun make-init-env (&optional env)
  (let ((htable (cadr env)))
    (if (null htable)
      (setf htable (make-hash-table)))

    (let ((expr-parse-table (make-hash-table))
          (essntials  `((:define . ,#'scheme-define)
                        (:if . ,#'scheme-if)
                        (:let . ,#'scheme-let)
                        (:fix . ,#'scheme-fix)))
          (primitives `((:+-two  . ,#'+-two)
                        (:--two  . ,#'--two)
                        (:>>-two . ,#'>>-two)
                        (:<<-two . ,#'<<-two)
                        (:<-two  . ,#'<-two)
                        (:>-two  . ,#'>-two)
                        (:>=-two . ,#'>=-two)
                        (:<=-two . ,#'<=-two)
                        (:=-two  . ,#'=-two)
                        (:heap . ,#'heap)
                        (:record-set! . ,#'record-set!)
                        (:record-ref . ,#'record-ref))))

      (dolist (src-i (list essntials primitives))
        (mapcar #'(lambda (pair)
                    (let ((key (car pair)) 
                          (value (cdr pair)))
                      (setf (gethash key expr-parse-table) value)))
                src-i))
      (setf (gethash :expr-parse-table htable) expr-parse-table))

    (if (null env)
      (setf env (list nil nil)))

    (setf (cadr env) htable)
    env))

;----------------------------------------------------------------
(defparameter *env* (make-init-env))
(defparameter *debug-mode* nil)

;----------------------------------------------------------------
(defun parse-mini-scheme (expr env &optional (debug-mode *debug-mode*))
  (if debug-mode
    (format t "expr:~s~%" expr))
  (if (terminal-p expr)
    (parse-expr-terminal expr env)
    (let* ((op (car expr))
           (expander (lookup-parser op :syntax-sugar-table env)))
      ;(format t "op:~s~%" op)
      (if expander
        (parse-mini-scheme (funcall expander expr env) env)
        (let ((parser (lookup-parser op :expr-parse-table env)))
          ;(format t "<~s> ~%" parser)
          (if parser
            (funcall parser expr env)
            (parser-app expr env)))))))
