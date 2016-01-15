;----------------------------------------------------------------
(defparameter *context* nil)
(defparameter *debug-mode* nil)
(defparameter *warning* t)
(defparameter *transfer-table* nil)
(defparameter *cps-gensym-debug* t)

;----------------------------------------------------------------
(defun primitive-warning (func-name)
  (if *warning*
    (if 
      (case func-name
        ('define t)
        ('if t)
        ('fix t)
        ('let t)
        ('fixh t)
        ('fixs t)
        (:fixh t)
        (:fixs t)
        (otherwise nil))
      (progn
        (warn (format nil "~%Warning!!!!~%Primitive???? ~a~%" func-name))
        (sleep 1)))))

;----------------------------------------------------------------
(defun compare-symbolp (sym)
  (case sym
    (:>  t)
    (:<  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)
    (otherwise nil)))

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


(defun variable-rename (value context)
  (let ((continuation-lambda (car context)))
    (labels ((find-renamed-value (value table-list)
             (if (null table-list) 
               value
               (let ((table (car table-list)))
                 (if (null table) 
                   (find-renamed-value value (cdr table-list))
                   (multiple-value-bind (renamed-value exist) (gethash value table)
                     (if exist
                       (progn 
                         (if (functionp continuation-lambda)
                           (setf (gethash value table) 
                                 (cons continuation-lambda renamed-value)))
                         :place-holder)
                       (find-renamed-value value (cdr table-list)))))))))
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
    (let ((arg0 (variable-rename expr context))

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
(defun make-exit-transfer-lambda ()
  (let* ((new-cps-expr (copy-tree `(:APP EXIT (ARG0))))
 
         (arg0-list (pickup-list new-cps-expr 'ARG0)))
    (flet ((fill-arg0 (arg0) (setf (car arg0-list) arg0)
                      new-cps-expr))
      #'fill-arg0)))

(defun make-exit-continuous ()
      (cons (make-exit-transfer-lambda) nil))

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
                     (:NEQ? (CLOUSE-RESULT ARG1) ()
                            (TRUE-CLOUSE FALSE-CLOUSE)))))
         (compare-expr 
           (copy-tree
             `(OP (ARG0 ARG1) () (TRUE-CLOUSE FALSE-CLOUSE))))

         (true-clouse-expr
           (copy-tree `(:APP ,inner-func-name (TRUE-RESULT-SYM))))

         (false-clouse-expr
           (copy-tree `(:APP ,inner-func-name (FALSE-RESULT-SYM))))

         (neq-list (pickup-list new-cps-expr :NEQ?))
         (cont-list (pickup-list new-cps-expr 'CONT))
         (arg1-list (pickup-list new-cps-expr 'ARG1))
         (clouse-list (pickup-list new-cps-expr 'CLOUSE-RESULT))
         (true-list (pickup-list new-cps-expr 'TRUE-CLOUSE))
         (false-list (pickup-list new-cps-expr 'FALSE-CLOUSE))
         cont-result

         compare-arg1-result
         clouse-result )

    ;(setf cl (pickup-list new-cps-expr 'CONT))
    ;(setf true-list (pickup-list new-cps-expr 'TRUE-CLOUSE))
    ;(setf false-list (pickup-list new-cps-expr 'FALSE-CLOUSE))

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)

           (fill-true-symbol (sym) (setf (caaddr true-clouse-expr) sym) true-clouse-expr)
           (fill-false-symbol (sym) (setf (caaddr false-clouse-expr) sym) false-clouse-expr)
           (fill-true-clouse (true-clouse) (setf (car true-list) true-clouse) new-cps-expr)
           (fill-false-clouse (false-clouse) (setf (car false-list) false-clouse) new-cps-expr)
           (fill-arg1 (arg1) (setf (car arg1-list) arg1) new-cps-expr)
           (fill-clouse-result (clouse) (setf (car clouse-list) clouse) compare-arg1-result))

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
        (let ((op (and (listp condition-expr) (car condition-expr))))
          (if (compare-symbolp op)
            (let ((arg0-expr (cadr condition-expr))
                  (arg1-expr (caddr condition-expr)))

              (setf compare-arg1-result
                    (do-lisp-to-cps arg1-expr (cons #'fill-arg1 table-list)))
              (setf (car neq-list) op)
              (do-lisp-to-cps arg0-expr (cons #'fill-clouse-result table-list)))

            (progn
              (setf compare-arg1-result new-cps-expr)
              (fill-arg1 :#f)
              (do-lisp-to-cps condition-expr (cons #'fill-clouse-result table-list)))))))))

;----------------------------------------------------------------
;(func-name (arg*) expr)

(defun fbind-transfer (fbind table-list)
  (let* ((func-name (car fbind))
         (args (cadr fbind))
         (func-expr (caddr fbind))
         (kont-sym (cps-gensym))

         (return-app (copy-tree `(:APP ,kont-sym (RESULT))))
         (new-cps-expr (copy-tree `(,func-name (,kont-sym ,@args)
                                       FUNC-BODY)))
         (func-body-list (pickup-list new-cps-expr 'FUNC-BODY))
         (result-list (pickup-list return-app 'RESULT)))

    (flet ((fill-body (func-body) (setf (car func-body-list) func-body) new-cps-expr)
           (fill-result (rv) (setf (car result-list) rv) return-app))

      (fill-body 
        (do-lisp-to-cps func-expr (cons #'fill-result table-list))))))

;----------------------------------------------------------------
(defun fix-transfer (expr context)
  (let ((cont-lambda (car context))
        (table-list (cdr context))

        (fbinds (cadr expr))
        (fix-expr (caddr expr)))

    (let* ((cps-binds (mapcar #'(lambda (fbind) 
                                  (fbind-transfer fbind table-list)) fbinds))
           (fix-expr0 (copy-tree `(FIX-EXPR))))

      (flet ((fill-fix-expr (expr0) (setf (car fix-expr0) expr0) fix-expr0))
        ;(print `(fix-transfer ,fix-expr ,fix-expr0 ,(cps-gensym)))

        `(:FIXH ,cps-binds ,
                           (do-lisp-to-cps fix-expr context))))))

;----------------------------------------------------------------
(defun apply-transfer (expr context)
  (primitive-warning (car expr))
  (let* ((return-sym (cps-gensym))
         (arg0 (cps-gensym))
         (func-name (car expr))
         (new-cps-expr (copy-tree `(:FIXS ((,return-sym (,arg0) CONT))
                                        (:APP FUNC-NAME ARGS))))
         (func-name-list (pickup-list new-cps-expr 'FUNC-NAME))
         (cont-list (pickup-list new-cps-expr 'CONT))
         (args-list (pickup-list new-cps-expr 'ARGS))
         (new-args nil)
         (args (cdr expr))
         wrapped-cps-expr)

    ;(print `('apply-transfer ,func-name ,(variable-rename func-name context)))

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
           (fill-func-name (func-name) (setf (car func-name-list) func-name) wrapped-cps-expr))

      ;(print expr)
      (let ((cont-lambda (car context))
            (table-list (cdr context))
            (args-holder 
               (cons return-sym
                     (mapcar #'(lambda (x) (if (atom x) x :APP-ARGS-PLACE-HOLDER)) args))))

        (setf (car args-list) args-holder)

        (let ((fill-arg-list
                (maplist #'(lambda (x) (flet ((fill-arg (arg)
                                                  (setf (car x) arg)
                                                  wrapped-cps-expr))
                                           #'fill-arg)) args-holder)))

          (setf wrapped-cps-expr 
                (do-lisp-to-cps func-name (cons #'fill-func-name table-list)))

          (setf wrapped-cps-expr
                (fill-cont (call-continuation-lambda cont-lambda arg0)))

          ;(print `(eval-args ,args-holder ,args ,(length fill-arg-list)))
          (mapc 
            #'(lambda (arg func)
                (setf wrapped-cps-expr
                      (do-lisp-to-cps arg (cons func table-list))))
            (reverse args) (nreverse (cdr fill-arg-list)))

          wrapped-cps-expr)))))

;----------------------------------------------------------------
(defun func-declare-transfer (expr context)
  (let* ((func-name (caadr expr))
         (args (cdadr expr))
         (kont-sym (cps-gensym))
         (return-app (copy-tree `(:APP ,kont-sym (RESULT))))
         (new-cps-expr (copy-tree 
                         `(:FIXH ((,func-name (,kont-sym ,@args) FUNC-BODY)) CONT)))
         (cont-list (pickup-list new-cps-expr 'CONT))
         (func-body-list (pickup-list new-cps-expr 'FUNC-BODY))
         (result-list (pickup-list return-app 'RESULT)))

    (flet ((fill-body (func-body) (setf (car func-body-list) func-body) new-cps-expr)
           (fill-result (rv) (setf (car result-list) rv) return-app)
           (fill-cont (cont) (setf (car cont-list) cont) new-cps-expr))

      (let ((expr-body (reverse (cddr expr)))

            (cont-lambda (car context))
            (new-context (cons #'fill-result (cdr context)))
            body-result)

        (dolist (expr0 expr-body)
          (setf body-result (do-lisp-to-cps expr0 new-context))
          (setf (car new-context) body-result))

        (fill-body body-result)

        (fill-cont (do-lisp-to-cps :unspecified context))))))

;----------------------------------------------------------------
(defun id-declare-transfer (expr context)
  (let* ((id (cadr expr))
         (new-cps-expr `(:ID (RESULT) (,id) CONT))
         (expr0 (caddr expr)))
    (flet ((fill-cont (cont) (setf (cadddr new-cps-expr) cont) new-cps-expr)
           (fill-result (rv) (setf (caadr new-cps-expr) rv) new-cps-expr))

      (let ((cont-lambda (car context))
            (table-list (cdr context)))

        (fill-cont (call-continuation-lambda cont-lambda :unspecified))
        (do-lisp-to-cps expr0 (cons #'fill-result table-list))))))

;----------------------------------------------------------------
(defun define-transfer (expr context)
  (if (listp (cadr expr))
    (func-declare-transfer expr context)
    (id-declare-transfer expr context)))

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
            (old-context (cons cont-lambda table-list))
            (let-args-reverse (reverse (cadr expr)))
            (let-body-reverse (reverse (cddr expr))))

        (dolist (let-arg let-args-reverse)
          (let ((let-arg-sym (car let-arg)))
            (setf (gethash let-arg-sym table) nil)))

        (dolist (let-body-one let-body-reverse)
          (setf result (do-lisp-to-cps let-body-one new-context))
          (setf (car new-context) result))

        (setf (car old-context) #'call-all-fill-placeholder)

        (dolist (let-arg let-args-reverse)
          (let ((let-arg-sym (car let-arg))
                (let-arg-body (cadr let-arg)))
            (setf key-is-let-arg-sym let-arg-sym)
            (setf result (do-lisp-to-cps let-arg-body old-context))))

        result))))

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
           (:>=  . ,#'>=-two)
           (:<=  . ,#'<=-two)
           (:=  . ,#'=-two)
           (:/=  . ,#'/=-two)

           (:heap . ,#'heap-transfer)
           (:record-set! . ,#'record-set!-transfer)
           (:record-ref . ,#'record-ref-transfer)))
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
          (:define (define-transfer expr context))
          (:if (if-transfer expr context))
          (:fix (fix-transfer expr context)) 
          (:let (let-transfer expr context)) 
          (:exit (exit-transfer expr context))

          (otherwise 
            (apply-transfer expr context)))))))
