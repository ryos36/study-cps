;----------------------------------------------------------------
; primitive

(defun +-two (expr context)
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

      (format t "do +:~a ~a result-sym:~a~%" arg0 arg1 result-sym)
      (format t "   cl:~a~%" cont-lambda)

      (setf cont-result
            (fill-cont (call-continuation-lambda cont-lambda result-sym)))

      (setf arg1-result
            (do-lisp-to-cps arg1 (cons #'fill-arg1 table-list)))

      (do-lisp-to-cps arg0 (cons #'fill-arg0 table-list))))))
