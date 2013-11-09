;----------------------------------------------------------------
(defun l ()
  (load "cps.lisp"))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or (symbolp expr)
      (numberp expr)))

;----------------------------------------------------------------
(defparameter *cps-symbol* (make-hash-table))

(defun lookup-symbol (key)
  (gethash table-key *cps-symbol*))

(defun parse-expr-terminal (expr)
  (if (symbolp expr)
    (lookup-symbol expr)
    expr))

;----------------------------------------------------------------
; primitive
;(load "cps-primitive.lisp")
(defun cps-+ (expr)
  (let ((ops (cadr expr))
        (ids (caddr expr))
        (next-expr (cadddr expr)))

  ))

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

(defun lookup-primitive (op table-key)
  (gethash table-key *primitive-table*))

;----------------------------------------------------------------
(define-condition cps-parse-error (error)
  ((expr :initarg :expr :reader expr) )
  (:report (lambda (condition stream)
             (progn
               (format *error-output* 
                       "Error !!:<~s>~%" (expr condition))))))

(defun error-exit (expr)
   (error 'cps-parse-error :expr expr))


;----------------------------------------------------------------
(defun cps-fix (expr)
  )

;----------------------------------------------------------------
(defun cps-app (expr)
  )

;----------------------------------------------------------------
(defparameter *debug-mode* nil)

;----------------------------------------------------------------
(defun parse-cps (expr &optional (debug-mode *debug-mode*))
  (if debug-mode
    (format t "expr:~s~%" expr))

  (if (terminal-p expr)
    (parse-expr-terminal expr)
    (let ((op (car expr)))
      (case op
        (:fix (cps-fix expr))
        (:app (cps-app expr))
        (otherwise (let ((primitive-cps (lookup-primitive op)))
                     (funcall primitive-cps expr)))))))

