;----------------------------------------------------------------
; primitive

(defmacro make-two-args-primitive (func-name op)
    `(defun ,func-name (expr context)
       (let* ((result-sym (cps-gensym))
              (new-cps-expr (copy-tree `(,,op (ARG0 ARG1) (,result-sym) (CONT))))
              (cont-list (pickup-list new-cps-expr 'CONT))
              arg1-result cont-result)
         (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
                (fill-arg1 (arg1) (setf (cadadr new-cps-expr) arg1) cont-result)
                (fill-arg0 (arg0) (setf (caadr new-cps-expr) arg0) arg1-result))

           (let ((arg0 (cadr expr))
                 (arg1 (caddr expr))

                 (cont-lambda (car context))
                 (table-list (cdr context)))

             (setf cont-result
                   (fill-cont (call-continuation-lambda cont-lambda result-sym)))

             (setf arg1-result
                   (do-lisp-to-cps arg1 (cons #'fill-arg1 table-list)))

             (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list)))))))

;----------------------------------------------------------------
(defmacro make-compare-primitive (func-name op)
    `(defun ,func-name (expr context)
       (let* ((result-sym (cps-gensym))
              (kont-sym (cps-gensym))
              (new-cps-expr (copy-tree `(:FIXS ((,kont-sym (,result-sym) CONT))
                                               (,,op (ARG0 ARG1) () 
                                                     (:APP ,kont-sym (:#t))
                                                     (:APP ,kont-sym (:#f))))))
              (cont-list (pickup-list new-cps-expr 'CONT))
              (arg1-list (pickup-list new-cps-expr 'ARG1))
              (arg0-list (pickup-list new-cps-expr 'ARG0))
              arg1-result cont-result)

         (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
                (fill-arg1 (arg1) (setf (car arg1-list) arg1) cont-result)
                (fill-arg0 (arg0) (setf (car arg0-list) arg0) arg1-result))

           (let ((arg0 (cadr expr))
                 (arg1 (caddr expr))

                 (cont-lambda (car context))
                 (table-list (cdr context)))

             (setf cont-result
                   (fill-cont (call-continuation-lambda cont-lambda result-sym)))

             (setf arg1-result
                   (do-lisp-to-cps arg1 (cons #'fill-arg1 table-list)))

             (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list)))))))
;----------------------------------------------------------------

(make-two-args-primitive --two :-)
(make-two-args-primitive +-two :+)
(make-two-args-primitive *-two :*)

(make-two-args-primitive >>-two :>>)
(make-two-args-primitive <<-two :<<)

(make-compare-primitive >-two :>)
(make-compare-primitive <-two :<)
(make-compare-primitive >=-two :>=)
(make-compare-primitive <=-two :<=)
(make-compare-primitive =-two :=)
(make-compare-primitive /=-two :/=)

;----------------------------------------------------------------
(defun heap-transfer (expr context)
  (let* ((result-sym (cps-gensym))
         (new-cps-expr `(:HEAP ARGS (,result-sym) (CONT)))
         (cont-list (pickup-list new-cps-expr 'CONT))
         (new-args nil)
         wrapped-cps-expr)

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
           (fill-arg (arg) (push arg new-args) wrapped-cps-expr))

      (let ((args (reverse (cdr expr)))

            (cont-lambda (car context))
            (table-list (cdr context)))

      (setf wrapped-cps-expr
            (fill-cont (call-continuation-lambda cont-lambda result-sym)))

      (dolist (arg args)
        (setf wrapped-cps-expr
              (do-lisp-to-cps arg `(,#'fill-arg ,table-list))))

      (setf (cadr new-cps-expr) new-args)
      wrapped-cps-expr))))

;----------------------------------------------------------------
(defun record-set!-transfer (expr context)
  (let* ((new-cps-expr (copy-tree `(:RECORD-SET! (RECORD-NAME ARG0 ARG1) () (CONT))))
         (record-name-list (pickup-list new-cps-expr 'RECORD-NAME))
         (arg0-list (pickup-list new-cps-expr 'ARG0))
         (arg1-list (pickup-list new-cps-expr 'ARG1))
         (cont-list (pickup-list new-cps-expr 'CONT))
         arg0-result
         arg1-result cont-result)

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
           (fill-arg1 (arg1) (setf (car arg1-list) arg1) cont-result)
           (fill-arg0 (arg0) (setf (car arg0-list) arg0) arg1-result)
           (fill-record-name (name) (setf (car record-name-list) name) arg0-result))

      (let ((record-name (cadr expr))
            (arg0 (caddr expr))
            (arg1 (cadddr expr))

            (cont-lambda (car context))
            (table-list (cdr context)))

        (setf cont-result
              (fill-cont (call-continuation-lambda cont-lambda :unspecified)))


        (setf arg1-result
              (do-lisp-to-cps arg1 (cons #'fill-arg1 table-list)))

        (setf arg0-result 
              (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list)))

        (do-lisp-to-cps record-name (cons #'fill-record-name table-list))))))
;----------------------------------------------------------------
(defun record-ref-transfer (expr context)
  (let* ((result-sym (cps-gensym))
         (new-cps-expr (copy-tree `(:RECORD-REF (RECORD-NAME ARG0) (,result-sym) (CONT))))
         (record-name-list (pickup-list new-cps-expr 'RECORD-NAME))

         (arg0-list (pickup-list new-cps-expr 'ARG0))
         (cont-list (pickup-list new-cps-expr 'CONT))
         arg0-result
         cont-result)

    (flet ((fill-cont (cont) (setf (car cont-list) cont) new-cps-expr)
           (fill-arg0 (arg0) (setf (car arg0-list) arg0) cont-result)
           (fill-record-name (name) (setf (car record-name-list) name) arg0-result))

      (let ((record-name (cadr expr))
            (arg0 (caddr expr))

            (cont-lambda (car context))
            (table-list (cdr context)))

        (setf cont-result
              (fill-cont (call-continuation-lambda cont-lambda result-sym)))

        (setf arg0-result
              (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list)))

        (do-lisp-to-cps record-name (cons #'fill-record-name table-list))))))

;----------------------------------------------------------------
#|
(defun old-+-two (expr context)
  (let* ((result-sym (cps-gensym))
         (new-cps-expr (copy-tree `(:+ (ARG0 ARG1) (,result-sym) (CONT))))
         cont-result arg1-result)

    (flet ((fill-arg0 (arg0) (setf (caadr new-cps-expr) arg0) arg1-result)
           (fill-arg1 (arg1) (setf (cadadr new-cps-expr) arg1) cont-result)
           (fill-cont (cont) (setf (caadddr new-cps-expr) cont) new-cps-expr))

      (let ((arg0 (cadr expr))
            (arg1 (caddr expr))

            (cont-lambda (car context))
            (table-list (cdr context)))

        ;(format t "do +:~a ~a result-sym:~a~%" arg0 arg1 result-sym)
        ;(format t "   cl:~a~%" cont-lambda)

        (setf cont-result
              (fill-cont (call-continuation-lambda cont-lambda result-sym)))

        (setf arg1-result
              (do-lisp-to-cps arg1 (cons #'fill-arg1 table-list)))

        (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list))))))
|#
