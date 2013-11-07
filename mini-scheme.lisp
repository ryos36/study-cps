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
(defun error-exit (expr env)
  (format *error-output* "error:~s~%" expr))

;----------------------------------------------------------------
; primitive
(load "primitive.lisp")

;----------------------------------------------------------------
(defun scheme-if (expr env)
  (let ((condition-s (cadr expr))
        (true-clouse (caddr expr))
        (false-clouse (cadddr expr)))
    (let ((cond-result (parse-mini-scheme condition-s env)))
      (if (not (eq :#f cond-result))
        (parse-mini-scheme true-clouse env)
        (if false-clouse 
          (parse-mini-scheme false-clouse env))))))

(defun scheme-let (expr env)
  (let ((binds (cadr expr))
        (body-expr (caddr expr))
        (htable (make-hash-table)))
    (dolist (arg-pair binds)
      (let ((sym (car arg-pair))
            (value-expr (car arg-pair)))
        (if (not (symbolp sym))
          (error-exit sym env))
        (setf (gethash sym htable) (parse-mini-scheme value-expr env))))
    (let ((new-env (list env htable)))
      (parse-mini-scheme body-expr new-env))))

(defun scheme-define (expr env)
  (let ((sym (cadr expr))
        (value (parse-mini-scheme (caddr expr) env)))
    (if (not (symbolp sym))
      (error-exit sym env))
    (setf (gethash sym (cadr env)) value)
    value ))

;----------------------------------------------------------------
(defun parser-app (expr env))

;----------------------------------------------------------------
(defun make-init-env (&optional env)
  (let ((htable (cadr env)))
    (if (null htable)
      (setf htable (make-hash-table)))

    (let ((expr-parse-table (make-hash-table))
          (essntials  `((:define . ,#'scheme-define)
                        (:if . ,#'scheme-if)
                        (:let . ,#'scheme-let)))
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
