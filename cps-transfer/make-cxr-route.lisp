(defun make-cxr-route (expr word)
  (labels ((make-cxr0 (expr0 result)
              (if (null expr0) nil
                (let ((result-l (cons 'a result)))
                  (if (eq expr0 word) result
                    (if (atom expr0) nil
                      (let ((first-expr0 (car expr0))
                            (remain-expr0  (cdr expr0))
                            (result-r (cons 'd result)))
                        (let ((rv (make-cxr0 first-expr0 result-l)))
                          (if rv rv
                            (make-cxr0 remain-expr0 result-r))))))))))

    (make-cxr0 expr '())))


(defun pickup-list (expr word &optional debug-print)
  (labels ((do-cxr (cxr expr0)
                   (if (null cxr) expr0
                     (if (atom expr0)
                       expr0
                       (let* ((op (if (eq 'a (car cxr)) #'car #'cdr))
                              (rv (apply op (list expr0))))
                         (if debug-print (print rv))
                         (do-cxr (cdr cxr) rv))))))

    (let ((rv-cxr (make-cxr-route expr word)))
      (if debug-print (print rv-cxr))
      (do-cxr (reverse (cdr rv-cxr)) expr))))


#|
(let ((expr '(:FIXS ((x-inner-func-name (x-clouse-result) CONT))
                    (:NEQ? (x-cont-result-sym :#f)
                           ((:APP x-inner-func-name (TRUE-CLOUSE))
                            (:APP x-inner-func-name (FALSE-CLOUSE)))))))
  (let ((e0 (print-route expr 'CONT))
        (e1 (print-route expr 'TRUE-CLOUSE))
        (e2 (print-route expr 'FALSE-CLOUSE)))

    (flet ((ee0 (a0) (setf (car e0) a0) expr)
           (ee1 (a0) (setf (car e1) a0) expr)
           (ee2 (a0) (setf (car e2) a0) expr))

      (ee0 'put-into-cont)
      (ee1 'put-into-true-clouse)
      (ee2 'put-into-false-clouse)
  (print expr))))
|#
