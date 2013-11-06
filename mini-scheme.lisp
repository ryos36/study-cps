;----------------------------------------------------------------
;(defvar *syntax-sugar-table* nil)
;(defvar *expr-parse-table* nil)

;----------------------------------------------------------------
(defun l ()
  (load "mini-scheme.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun parse-expr-terminal (expr env)
  (if (numberp expr)
    expr
    (cadr (assoc expr env))))

;----------------------------------------------------------------
(defun lookup-parser (op table-key env)
  (let ((table (assoc table-key env)))
    (if (nullp table)
      nil
      (cadr (assoc op table)))))

;----------------------------------------------------------------
(defun mini-scheme (expr env)
  (if (terminal-p expr)
    (parse-expr-terminal expr env)
    (let* ((op (car expr))
           (expander (lookup-parser op :syntax-sugar-table env)))
      (if expander
        (mini-scheme (expander expr env) env)
        (let ((parser (lookup-parser op :expr-parse-table env)))
          (if parser
            (parser expr)
            (parser-app expr)))))))

