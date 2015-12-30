
;'(FB (0 1) (1 1) (N (+ (FB (- n 1)) (FB (- n 2)))))

(defun get-oldn (func-name expr)
  (labels ((do-expand (expr0)
                      (if (atom expr0) expr0
                        (let ((func (car expr0)))
                          (if (eq #'make-symbol func)
                            ;(print `(apply #'make-symbol ,expr0) )
                            (apply #'make-symbol `(,(cdr expr0)))
                            (mapcar #'do-expand expr0)))))
           (make-symbol (num)
                      (intern (format nil "~a-~a" func-name num)))
         (get-oldn0 (expr0 rv)
                      ;(print `(expr0 ,expr0))
                      (if (atom expr0) rv
                        (let ((op (car expr0))
                              (arg0 (cadr expr0))
                              (arg1 (caddr expr0)))

                          ;(print `(cadr ,arg0))
                          (let ((num
                                  (if (eq op func-name)
                                    (if (listp arg0)
                                      (let ((a0-op (car arg0))
                                            (a0-remain (cdr arg0)))
                                        (if (eq a0-op :-)
                                          (if (listp a0-remain)
                                            (let 
                                              ((a0-arg0 (car a0-remain))
                                               (a0-arg1 (cadr a0-remain)))
                                              (if (and (eq a0-arg0 'n)
                                                       (numberp a0-arg1)
                                                       (>= a0-arg1 0))
                                                a0-arg1)))))))))
                            ;(print `(num ,num ,rv))
                            (if num 
                              (progn
                                (setf (car expr0) #'make-symbol)
                                (setf (cdr expr0) num)
                                (push num rv)
                                rv)
                              (let ((new-rv (get-oldn0 arg0 rv)))
                                (get-oldn0 arg1 new-rv))))))))
    (let* ((new-expr (copy-tree expr))
           (rv (sort (get-oldn0 new-expr '()) #'<))
           (rv-expr (do-expand new-expr)))
      (cons rv-expr rv))))

(defun rec-opt (rec-def)
  (let ((func-name (car rec-def))
        (cond-list (sort (nreverse (cdr (reverse (cdr rec-def))))
                         #'(lambda (a0 a1)
                             (> (car a0) (car a1)))))
        (func-def (car (last rec-def))))

    (labels ((fill-list (n n-list rv)
                        (if (null n-list) (nreverse rv)
                          (let ((new-n-list 
                                  (if (= n (car n-list))
                                    (cdr n-list)
                                    n-list)))
                            (push n rv)
                            (fill-list (+ n 1) new-n-list rv))))

             (make-args (n-list)
                        (let ((new-n-list (fill-list 1 n-list '())))
                          (mapcar #'(lambda(x)
                                      (intern (format nil "~a-~a" func-name x)))
                                  new-n-list)))
             (make-if-if (clist body)
                         (if (null clist) body
                           (let ((one (car clist)))
                             (make-if-if (cdr clist)
                                         `(:if (:= n ,(car one)) ,(cadr one) ,body)))))

             (make-if-sym (sym)
                          `(,sym (:- n 1) (:+ n 1) ,@(mapcar #'(lambda (x) (cadr x)) cond-list)))
             (make-if (sym)
                      (make-if-if cond-list (make-if-sym sym))))

      (let* ((expr-oldn-list (get-oldn func-name (cadr func-def)))
             (expr (car expr-oldn-list))
             (oldn-list (cdr expr-oldn-list))
             (old-args (make-args oldn-list))
             (old-old-args (nreverse (cdr (reverse old-args))))
             (sym (intern (format nil "~a0" func-name)))
             (sym-0 (intern (format nil "~a-0" func-name)))
             (if-expr (make-if sym))

             (new-expr `(:FIX ((,sym (i n ,sym-0 ,@old-args) 
                                     (:if (:= i 0) ,sym-0
                                          (,sym (:- i 1)
                                                (:+ n 1)
                                                ,expr
                                                ,@old-old-args)))
                               (,func-name (n)
                                 ,if-expr))
                              (,func-name 8))))
        new-expr))))

(let ((e '(FB (1 1) (0 1) (N (:+ (FB (:- n 1)) (FB (:- n 2)))))))
  (print (rec-opt e)))

(let ((e '(my-fib (2 2) (1 1) (0 1) (3 3) (N (:* (my-fib (:- n 3)) (my-fib (:- n 4))))))) (print (rec-opt e)))
