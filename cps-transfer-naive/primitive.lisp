;----------------------------------------------------------------
; primitive

(defmacro primitive-2 (expr env op)
  `(let ((arg0 (cadr expr))
         (arg1 (caddr expr))

         (arg0-sym (cps-gensym))
         (arg1-sym (cps-gensym))
         (result-sym (car env))
         (continuation (cdr env)))

     (do-lisp-to-cps arg0 (cons arg0-sym 
       (do-lisp-to-cps arg1 (cons arg1-sym
         (copy-tree `(,,op (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))

(defmacro primitive-comparison (expr env op) 
  `(let ((arg0 (cadr expr))
         (arg1 (caddr expr))

         (arg0-sym (cps-gensym))
         (arg1-sym (cps-gensym))
         (result-sym (car env))
         (continuation (cdr env)))

     (do-lisp-to-cps arg0 (cons arg0-sym 
       (do-lisp-to-cps arg1 (cons arg1-sym
         (copy-tree `(,,op (,arg0-sym ,arg1-sym) (,result-sym) ((,continuation :#t)(,continuation :#f))))))))))


(defun +-two (expr env)
  (primitive-2 expr env :+))

(defun --two (expr env) 
  (primitive-2 expr env :-))

(defun >>-two (expr env) 
  (primitive-2 expr env :>>))

(defun <<-two (expr env) 
  (primitive-2 expr env :<<))

(defun >-two (expr env)
  (primitive-comparison expr env :>))

(defun <-two (expr env)
  (primitive-comparison expr env :<))

(defun >=-two (expr env)
  (primitive-comparison expr env :>=))

(defun <=-two (expr env)
  (primitive-comparison expr env :<=))

(defun =-two (expr env)
  (primitive-comparison expr env :=))


#|

(defun x+-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:+ (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))

(defun =-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:= (,arg0-sym ,arg1-sym) () ((,continuation :#t)(,continuation :#f))))))))))


(defun +-two (expr env)
  (primitive-2 expr env :+))

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



(defun x+-two (expr env) 
  (let ((arg0 (cadr expr))
        (arg1 (caddr expr))

        (arg0-sym (cps-gensym))
        (arg1-sym (cps-gensym))
        (result-sym (car env))
        (continuation (cdr env)))

    (do-lisp-to-cps arg0 (cons arg0-sym 
      (do-lisp-to-cps arg1 (cons arg1-sym
        (copy-tree `(:+ (,arg0-sym ,arg1-sym) (,result-sym) (,continuation)))))))))
|#
