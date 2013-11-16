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
    (let ((table (cdr env)))
      (if (null table)
        (lookup-symbol key (car env))

        (multiple-value-bind
          (result exist) (gethash key table)

          (if (null exist)
            (lookup-symbol key (car env))
            result))))))

(defun set-key-value (key value env)
  (let ((table (cdr env)))
    (setf (gethash key table) value)))

(defun parse-expr-terminal (expr env)
  (if (symbolp expr)
    (case expr
      (:#t :#t) ; simulate scheme style #t
      (:#f :#f) ; simulate scheme style #f
      (otherwise (let ((result (lookup-symbol expr env)))
                   (if (eq result :not-found) (lisp-error-exit expr env)
                     (if (symbolp result) (parse-expr-terminal result env)
                       result)))))
    expr))

;----------------------------------------------------------------
(defun lookup-parser (op table-key env)
  (if (null env) nil
    (let ((table (gethash table-key (cdr env))))
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
; func-closure = ( func-define . env )
; func = (func-closure v0 v1 v2 ....)
;
(defun parser-app (expr g-env)
  (let (result
         (func-closure (car expr))
         (values (cdr expr)))
    (if (symbolp func-closure)
      (setf func-closure (lookup-symbol func-closure g-env)))


    (if (eq func-closure :not-found)
      (lisp-error-exit expr g-env))

    (let ((func-define (car func-closure))
          (env (cdr func-closure)))

      (let ((args (cadr func-define))
            (body-expr-list (cddr func-define))
            (htable (cdr env))
            value)

        (if (not (= (length args) (length values)))
          (lisp-error-exit expr env))

        (map nil #'(lambda (arg value)
                     (if (not (symbolp arg))
                       (lisp-error-exit expr env))
                     (set-key-value arg (parse-mini-lisp value g-env) env))
                     args values)

        (dolist (body-expr body-expr-list)
          (setf result
                (parse-mini-lisp body-expr env)))
        result))))

;----------------------------------------------------------------
(defun make-func-define (name args body)
  (copy-tree `(,name ,args ,@body)))

(defun make-func-closure (func-define env)
  (cons func-define env))

(defun make-app-function (func-closure values)
  (cons func-closure values))

(defun make-new-env (env)
  (cons env (make-hash-table)))

(defun make-init-env (&optional env)
  (let ((htable (cdr env)))
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
      (setf env (cons nil nil)))

    (setf (cdr env) htable)
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
           (expander (lookup-parser op :syntax-sugar-table *env*)))
      ;(format t "op:~s~%" op)
      (if expander
        (parse-mini-lisp (funcall expander expr env) env)
        (let ((parser (lookup-parser op :expr-parse-table *env*)))
          ;(format t "parser:<~s> ~%" parser)
          (if parser
            (funcall parser expr env)
            (parser-app expr env)))))))
