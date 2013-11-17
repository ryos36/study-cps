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
  (or (null expr)
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
(format t "terminal-transfer ~a~%" expr)
    (let ((arg0 (valiable-rename expr context))

          (cont-lambda (car context)))

      (call-continuation-lambda cont-lambda arg0)))


;----------------------------------------------------------------
(let ((no 0))
 (defun cps-gensym () 
   (if *cps-gensym-debug*
     (intern (format nil "sym~a" (incf no)))
     (gensym))))

;----------------------------------------------------------------
(load "primitive.lisp")

;----------------------------------------------------------------
(defun make-exit-continuous ()
  (let ((new-cps-expr (copy-list `(:exit (ARG0) () ()))))
    (flet ((fill-arg0 (arg0) (setf (caadr new-cps-expr) arg0) new-cps-expr))

      (cons #'fill-arg0 nil))))

;----------------------------------------------------------------
(defun exit-transfer (expr context)
  (flet ((exit-transfer-lambda (r0)
           (copy-tree `(:exit (,r0) () ()))))

  (let ((arg0 (cadr expr))
        (table-list (cdr context)))
    (do-lisp-to-cps arg0 (cons #'exit-transfer-lambda table-list)))))

;----------------------------------------------------------------
;(func-name (arg*) expr)


;----------------------------------------------------------------

(setf my-result nil)
(setf key nil)
(setf my-table nil)
(defun my-callback (new-sym)
  (let ((callbacks (gethash key my-table)))
    (dolist (callback callbacks)
      (funcall callback new-sym)))
   my-result)

(defun let-transfer (expr context)

  (let ((cont-lambda (car context))
        (table-list (cdr context))
        (table (make-hash-table)))

    (let ((new-context (cons cont-lambda (cons table table-list)))
          (let-args-reverse (reverse (cadr expr)))
          (let-body-reverse (reverse (cddr expr)))
          result)

      (dolist (let-arg let-args-reverse)
        (let ((let-arg-sym (car let-arg)))
          (setf (gethash let-arg-sym table) nil)))

      (dolist (let-body-one let-body-reverse)
        (setf result (do-lisp-to-cps let-body-one new-context))
        (setf (car new-context) result))

(format t "XXXXXXXXXXXXXXXresult:~a~%" result)

      (setf my-result result)
      (setf my-table table)
      (setf (car new-context) #'my-callback)

      (dolist (let-arg let-args-reverse)
        (let ((let-arg-sym (car let-arg))
              (let-arg-body (cadr let-arg)))
          (setf key let-arg-sym)
          (setf my-result (do-lisp-to-cps let-arg-body new-context))))
      my-result)))

;----------------------------------------------------------------


;----------------------------------------------------------------
(defun make-transfer-table ()
  (let ((table (make-hash-table)))
    (map nil #'(lambda (x) 
                 (let ((op (car x))
                       (func (cdr x)))
                   (setf (gethash op table) func)))
         `((:+ . ,#'+-two)
           #|
           (:- . ,#'--two)
           (:>> . ,#'>>-two)
           (:<< . ,#'<<-two)

           (:> . ,#'>-two)
           (:< . ,#'<-two)
           (:>= . ,#'>=-two)
           (:<= . ,#'<=-two)
           (:= . ,#'=-two)
           |#
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
          ;(:if (if-transfer expr context))
          ;(:fix (fix-transfer expr context)) 
          (:let (let-transfer expr context)) 
          (:exit (exit-transfer expr context))

          (otherwise nil))))))

