;----------------------------------------------------------------
(defparameter *context* nil)
(defparameter *debug-mode* nil)
(defparameter *transfer-table* nil)
(defparameter *cps-gensym-debug* t)

;----------------------------------------------------------------
(defun l ()
  (load "lisp-to-cps.lisp"))

;----------------------------------------------------------------
(defun caadddr (tree)
         (car (cadddr tree)))

(defun caadddr-set (tree value)
  (setf (car (cadddr tree)) value))

(defsetf caadddr caadddr-set)

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or 
    (eq :#t expr)
    (eq :#f expr)
    (null expr)
    (symbolp expr)
    (numberp expr)))

;----------------------------------------------------------------
;
; context := (continuation-lambda . (tablen ... table1 table0))
;


(defun valiable-rename (value context)
  (let ((continuation-lambda (car context)))
    (labels ((find-renamed-value (value table-list)
             (if (null table-list) 
               value
               (let ((table (car table-list)))
                 (if (null table) 
                   (find-renamed-value (value (cdr table-list)))
                   (multiple-value-bind (renamed-value exist) (gethash value table)
                     (if exist
                       (progn 
                         (setf (gethash value table) 
                               (cons continuation-lambda (gethash value table)))
                         :place-holder)
                       (find-renamed-value (value (cdr table-list))))))))))
    (cond
      ((null value) nil) 
      ((eq value :#t) :#t)
      ((eq value :#f) :#f)
      ((numberp value) value)
      (t (let ((table-list (cdr context))) 
           (find-renamed-value value table-list)))))))

;----------------------------------------------------------------
(defun call-continuation-lambda (cont-lambda arg0)
  (if cont-lambda 
    (if (functionp cont-lambda)
      (funcall cont-lambda arg0)
      cont-lambda)
    arg0))

;----------------------------------------------------------------
(defun terminal-transfer (expr context)
    (let ((arg0 (valiable-rename expr context))

          (cont-lambda (car context)))

      (call-continuation-lambda cont-lambda arg0)))

;----------------------------------------------------------------
(let ((no 0))
 (defun cps-gensym (&optional do-init )
   (if do-init (setf no (if (numberp do-init) do-init 0)))
   (if *cps-gensym-debug*
     (intern (format nil "sym~a" (incf no)))
     (gensym))))

;----------------------------------------------------------------
(load "make-cxr-route.lisp")
(load "primitive.lisp")

;----------------------------------------------------------------
(defmacro make-exit-transfer-lambda ()
  (let ((new-cps-expr '`(:exit (,arg0) () ())))
    `(lambda (arg0)
       (let ((new-cps-expr ,new-cps-expr))
         new-cps-expr))))

(defun make-exit-continuous ()
      (cons (make-exit-transfer-lambda) nil))

;----------------------------------------------------------------
(defun exit-transfer (expr context)
  (let ((arg0 (cadr expr))
        (table-list (cdr context)))
    (do-lisp-to-cps arg0 (cons (make-exit-transfer-lambda) table-list))))

;----------------------------------------------------------------
(defun if-transfer (expr context)
  (let* ((inner-func-name (cps-gensym))
         (cont-result-sym (cps-gensym))
         (new-cps-expr 
           (copy-tree 
             `(:FIXS ((,inner-func-name (,cont-result-sym) CONT))
                     (:NEQ? (CLOUSE-RESULT :#f) ()
                            (TRUE-CLOUSE FALSE-CLOUSE)))))
         (true-clouse-expr
          (copy-tree `(:APP ,inner-func-name (TRUE-RESULT-SYM))))

         (false-clouse-expr
          (copy-tree `(:APP ,inner-func-name (FALSE-RESULT-SYM))))

         (cont-list (pickup-list new-cps-expr 'CONT))
         (clouse-list (pickup-list new-cps-expr 'CLOUSE-RESULT))
         (true-list (pickup-list new-cps-expr 'TRUE-CLOUSE))
         (false-list (pickup-list new-cps-expr 'FALSE-CLOUSE))
         cont-result

         clouse-result
         true-result)

    ;(setf cl (pickup-list new-cps-expr 'CONT))
    ;(setf true-list (pickup-list new-cps-expr 'TRUE-CLOUSE))
    ;(setf false-list (pickup-list new-cps-expr 'FALSE-CLOUSE))

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
           (fill-true-symbol (sym) (setf (caaddr true-clouse-expr) sym) true-clouse-expr)
           (fill-false-symbol (sym) (setf (caaddr false-clouse-expr) sym) false-clouse-expr)
           (fill-true-clouse (true-clouse) (setf (car true-list) true-clouse) new-cps-expr)
           (fill-false-clouse (false-clouse) (setf (car false-list) false-clouse) new-cps-expr)
           (fill-clouse-result (clouse) (setf (car clouse-list) clouse) new-cps-expr))

      (let ((cont-lambda (car context))
            (table-list (cdr context))

            (condition-expr (cadr expr))
            (true-clouse (caddr expr))
            (false-clouse (cadddr expr)))

        (fill-cont (call-continuation-lambda cont-lambda cont-result-sym))
        (fill-true-clouse
              (do-lisp-to-cps true-clouse (cons #'fill-true-symbol table-list)))
        (fill-false-clouse
              (do-lisp-to-cps false-clouse (cons #'fill-false-symbol table-list)))
        (do-lisp-to-cps condition-expr (cons #'fill-clouse-result table-list))))))

;----------------------------------------------------------------
(defun let-transfer (expr context)
  (let ((cont-lambda (car context))
        (table-list (cdr context))
        (table (make-hash-table))
        key-is-let-arg-sym
        result)

    (flet ((call-all-fill-placeholder (new-sym)
              (let ((callbacks (gethash key-is-let-arg-sym table)))
                (dolist (callback callbacks)
                  (funcall callback new-sym)))
              result))

      (let ((new-context (cons cont-lambda (cons table table-list)))
            (let-args-reverse (reverse (cadr expr)))
            (let-body-reverse (reverse (cddr expr))))

        (dolist (let-arg let-args-reverse)
          (let ((let-arg-sym (car let-arg)))
            (setf (gethash let-arg-sym table) nil)))

        (dolist (let-body-one let-body-reverse)
          (setf result (do-lisp-to-cps let-body-one new-context))
          (setf (car new-context) result))

        (setf (car new-context) #'call-all-fill-placeholder)

        (dolist (let-arg let-args-reverse)
          (let ((let-arg-sym (car let-arg))
                (let-arg-body (cadr let-arg)))
            (setf key-is-let-arg-sym let-arg-sym)
            (setf result (do-lisp-to-cps let-arg-body new-context))))
        result))))

;----------------------------------------------------------------


;----------------------------------------------------------------
(defun make-transfer-table ()
  (let ((table (make-hash-table)))
    (map nil #'(lambda (x) 
                 (let ((op (car x))
                       (func (cdr x)))
                   (setf (gethash op table) func)))
         `((:+ . ,#'+-two)
           (:- . ,#'--two)
           (:* . ,#'*-two)

           (:>> . ,#'>>-two)
           (:<< . ,#'<<-two)

           (:>  . ,#'>-two)
           (:<  . ,#'<-two)
           (:>= . ,#'>=-two)
           (:<= . ,#'<=-two)
           (:=  . ,#'=-two)
           ))
    table))

;----------------------------------------------------------------
(defun do-lisp-to-cps (expr context)
  (if *debug-mode*
    (format t "expr:~s~%" expr))
  (if (terminal-p expr)
    (terminal-transfer expr context)
    (let* ((op (car expr))
           (transfer (gethash op *transfer-table*)))
      (if transfer
        (funcall transfer expr context)

        (case op
          (:if (if-transfer expr context))
          ;(:fix (fix-transfer expr context)) 
          (:let (let-transfer expr context)) 
          (:exit (exit-transfer expr context))

          (otherwise nil))))))
