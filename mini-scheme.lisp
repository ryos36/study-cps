;----------------------------------------------------------------
(defun l ()
  (load "mini-scheme.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (null expr)
      (eq t expr)
      (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defun parse-expr-terminal (expr env)
  (if (symbolp expr)
    (gethash expr env)
    expr))

;----------------------------------------------------------------
(defun lookup-parser (op table-key env)
  (let ((table (gethash table-key env)))
    (if (null table)
      nil
      (gethash op table))))

;----------------------------------------------------------------
(defun error-exit (expr env)
  (format *error-output* "error:~s~%" expr))

(defun parser-app (expr env))

;----------------------------------------------------------------
; primitive
(load "primitive.lisp")
(defun +-two (expr env) (+ 
                          (parse-mini-scheme (cadr expr) env)
                          (parse-mini-scheme (caddr expr) env)))

(defun --two (expr env) (- (cadr expr) (caddr expr)))
(defun >>-two (expr env) 
  (labels ((>> (x r)
               (if (= r 0) x
                     (>> (floor (/ x 2)) (- r 1)))))
          (>> (cadr expr) (caddr expr))))

(defun <<-two (expr env) 
  (labels ((<< (x r)
               (if (= r 0) x
                 (<< (* x 2) (- r 1)))))
    (<< (cadr expr) (caddr expr))))

(defun >-two (expr env) (> (cadr expr) (caddr expr)))
(defun <-two (expr env) (< (cadr expr) (caddr expr)))
(defun >=-two (expr env) (>= (cadr expr) (caddr expr)))
(defun <=-two (expr env) (<= (cadr expr) (caddr expr)))
(defun =-two (expr env) (= (cadr expr) (caddr expr)))

; heap-record is '(:heap 1 2 3 ... )
(defun heap (expr env)
  (copy-list expr))

(defun record-ref (expr env)
  (let ((heap-record (cadr expr))
        (pos (caddr expr)))
    (incf pos)
    (nth pos heap-record)))

(defun record-set! (expr env)
  (let ((heap-record (cadr expr))
        (pos (caddr expr))
        (value (caddr expr)))
    (incf pos)
    (setf (nth pos heap-record) value)))

;----------------------------------------------------------------
(defun define (expr env)
  (let ((sym (cadr expr))
        (value (parse-mini-scheme (caddr expr) env)))
    (if (not (symbolp sym))
      (error-exit sym env))
    (setf (gethash sym env) value)
    value ))

;----------------------------------------------------------------
(defun make-init-env (&optional env)
  (if (null env) (setf env (make-hash-table)))
  (let ((expr-parse-table (make-hash-table))
        (essntials  `((:define . ,#'define)))
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
    (setf (gethash :expr-parse-table env) expr-parse-table)
    env))


;----------------------------------------------------------------
(defparameter *env* (make-init-env))

;----------------------------------------------------------------
(defun parse-mini-scheme (expr env)
  ;(format t "expr:~s~%" expr)
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
