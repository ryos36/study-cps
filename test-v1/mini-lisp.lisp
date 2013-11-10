;----------------------------------------------------------------
(defun l ()
  (load "mini-lisp.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (null expr)
      (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun lookup-symbol (key env)
  (if (null env) :not-found
    (let ((table (cadr env)))
      (if (null table)
        (lookup-symbol key (car env))

        (multiple-value-bind
          (result exist) (gethash key table)

          (if (null exist)
            (lookup-symbol key (car env))
            result))))))

(defun parse-expr-terminal (expr env)
  (if (symbolp expr)
    (case expr
      (:#t :#t)   ; simulate scheme style #t
      (:#f :#f) ; simulate scheme style #f
      (otherwise (let ((result (lookup-symbol expr env)))
                   (if (symbolp result) (parse-expr-terminal result env)
                     (if (eq result :not-found) (lisp-error-exit expr env)
                       result)))))
    expr))

;----------------------------------------------------------------
(defun lookup-parser (op table-key env)
  (if (null env) nil
    (let ((table (gethash table-key (cadr env))))
      (if (null table)
        (lookup-parser op table-key (car env))
        (gethash op table)))))

;----------------------------------------------------------------
(define-condition lisp-parse-error (error)
  ((expr :initarg :expr :reader expr) 
   (env :initarg :env :reader env))
  (:report (lambda (condition stream)
             (progn
               (format *error-output* 
                       "Error !!:<~s>~%" (expr condition))))))

(defun lisp-error-exit (expr env)
   (error 'lisp-parse-error :expr expr :env env))

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
    (if (eq func-define :not-found)
      (lisp-error-exit expr env))
    (let ((args (car func-define))
          (body-expr-list (cdr func-define))
          (htable (make-hash-table))
          value)
      (dolist (arg args)
        (setf value (car values))
        (setf values (cdr values))
        (if (not (symbolp arg))
          (lisp-error-exit sym env))
        (setf (gethash arg htable) (parse-mini-lisp value env)))

      (let ((new-env (list env htable)) r)
        (dolist (body-expr body-expr-list)
          (setf result
                (parse-mini-lisp body-expr new-env))
          )))
    result))

;----------------------------------------------------------------
(defun make-init-env (&optional env)
  (let ((htable (cadr env)))
    (if (null htable)
      (setf htable (make-hash-table)))

    (let ((expr-parse-table (make-hash-table))
          (essntials  `((:define . ,#'lisp-define)
                        (:if . ,#'lisp-if)
                        (:let . ,#'lisp-let)
                        (:fix . ,#'lisp-fix)))
          (primitives `((:+  . ,#'+-two)
                        (:-  . ,#'--two)
                        (:>> . ,#'>>-two)
                        (:<< . ,#'<<-two)
                        (:<  . ,#'<-two)
                        (:>  . ,#'>-two)
                        (:>= . ,#'>=-two)
                        (:<= . ,#'<=-two)
                        (:=  . ,#'=-two)
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
(defun parse-mini-lisp (expr env)
  (if *debug-mode*
    (format t "expr:~s~%" expr))
  (if (terminal-p expr)
    (parse-expr-terminal expr env)
    (let* ((op (car expr))
           (expander (lookup-parser op :syntax-sugar-table env)))
      ;(format t "op:~s~%" op)
      (if expander
        (parse-mini-lisp (funcall expander expr env) env)
        (let ((parser (lookup-parser op :expr-parse-table env)))
          ;(format t "<~s> ~%" parser)
          (if parser
            (funcall parser expr env)
            (parser-app expr env)))))))
