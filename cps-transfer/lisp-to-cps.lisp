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

  (labels ((find-renamed-value (value table-list)
             (if (null table-list) 
               value
               (let ((table (car table-list)))
                 (if (null table) 
                   (find-renamed-value (value (cdr table-list)))
                   (let ((renamed-value (gethash value table)))
                     (if renamed-value
                       renamed-value
                       (find-renamed-value (value (cdr table-list))))))))))

    (cond
      ((null value) nil) 
      ((eq value :#t) :#t)
      ((eq value :#f) :#f)
      ((numberp value) value)
      (t (let ((table-list (cdr context))) 
           (find-renamed-value value table-list))))))

;----------------------------------------------------------------
(defun terminal-transfer (expr context)
    (let ((arg0 (valiable-rename expr context))

          (cont-lambda (car context)))

      (funcall cont-lambda arg0)))


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
      (:exit (exit-transfer expr context))

      (otherwise nil))))))

