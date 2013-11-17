;----------------------------------------------------------------
; primitive

(defun +-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:+ (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))

(defun --two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:- (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))

(defun =-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:= (,arg0-sym ,arg1-sym) () ((,continuation :#t)(,continuation :#t))))))))))

#|
(defmacro primitive-2 (expr env &rest body)
  `(let ((arg0 (cadr ,expr))
        (arg1 (caddr ,expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car ,env))
        (continuation (cdr ,env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `,,@body)))))))

(defun +-two (expr env)
  (primitive-2 expr env 
               (:+ (arg0 arg1) (result) (cotinuation))))
|#

