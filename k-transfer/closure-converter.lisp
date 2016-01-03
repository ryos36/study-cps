(load "free-variable-finder.lisp")

;----------------------------------------------------------------
(defclass closure-converter (cps-parser)
  ())

;----------------------------------------------------------------
;(def-cps-func cps-bind ((parser closure-converter) expr env))

;----------------------------------------------------------------
(defun make-new-func-name (old-func-name)
  (intern (format nil ":~a" old-func-name)))

;----------------------------------------------------------------
;((:fix v0 v1 v2 ....) ...)
(defun env-to-free-variables (env)
  (cdar env))

;----------------------------------------------------------------
(defun wrap-cps-with-stack (func-names new-next-cps new-env)
  (let ((free-vars (env-to-free-variables new-env)))
    (labels ((wrap-cps-with-stack0 (func-names0 next-cps0)
               (if (null func-names0) next-cps0
                 (let* ((closure-name (car func-names0))
                        (label0 `(:LABEL ,(make-new-func-name closure-name)))
                        (stack-list (cons label0 
                                          (copy-list free-vars))))
                   (wrap-cps-with-stack0 (cdr func-names0) `(:STACK ,stack-list (,closure-name) (,new-next-cps)))))))
    (wrap-cps-with-stack0 (reverse func-names) new-next-cps))))

;----------------------------------------------------------------
(defun wrap-cps-with-record-ref (closure-name new-next-cps new-env)
  (let ((free-vars (env-to-free-variables new-env)))
    (labels ((wrap-cps-with-record-ref0 (var-names0 no next-cps0)
               (if (null var-names0) next-cps0
                 (let ((var-name (car var-names0)))
                   (wrap-cps-with-record-ref0 (cdr var-names0) (+ no 1) 
                     `(,:RECORD-REF (,closure-name ,no) (,var-name) ((,next-cps0))))))))
      (wrap-cps-with-record-ref0 free-vars 1 new-next-cps))))


;----------------------------------------------------------------
(def-cps-func cps-app ((parser closure-converter) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))

    (let ((closure-name func-name)
          (label0 `(label ,(make-new-func-name func-name)))
          (sym0 (cps-gensym parser)))

      `(:RECORD-REF (,closure-name 0) (,sym0) ((:APP ,sym0 (,closure-name ,@args)))))))

;----------------------------------------------------------------
#|
(def-cps-func cps-bind ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (print `(func-name-args ,args))

    (let ((func-name (make-new-func-name closure-name))
          (new-args (cons closure-name args))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,new-args ,(wrap-cps-with-record-ref closure-name new-next-cps env)))))
|#

;----------------------------------------------------------------
(def-cps-func cps-bind-fixs ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (free-vars (env-to-free-variables env)))

    (let* ((func-name (make-new-func-name closure-name))
           (new-args (cons closure-name args))
           (new-next-cps (cps-parse parser next-cps env))
           (pop-cps `(:POP (,(+ (length free-vars) 1)) () (,new-next-cps)))
           (wrapped-cps (wrap-cps-with-record-ref closure-name pop-cps env)))

      `(,func-name ,new-args ,wrapped-cps))))

;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '())))
      (dolist (bind binds)
        (cps-bind finder bind finder-env))

      (let* ((free-variables
              (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) (car finder-env))))
            (new-env (make-new-env parser env (cons `(:fixs) free-variables))))

        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind new-env)) binds))
              (new-next-cps (cps-parse parser next-cps new-env))
              (func-names (mapcar #'(lambda (x) (car x)) binds)))

          (let ((wrapped-cps (wrap-cps-with-stack func-names new-next-cps new-env)))
            `(,fix-op ,new-binds ,wrapped-cps)))))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixh ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (free-vars (env-to-free-variables env)))

    (let* ((func-name (make-new-func-name closure-name))
           (new-args (cons closure-name args))
           (new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,new-args ,new-next-cps))))

;----------------------------------------------------------------
(defun wrap-cps-with-heap (func-names heap-closure-sym expr)
  (labels ((wrap-cps-with-heap0 (func-names0 expr0)
             (if (null func-names0) expr0
               (let* ((closure-name (car func-names0))
                      (label0 `(:LABEL ,(make-new-func-name closure-name)))
                      (heap-list `(,label0 ,heap-closure-sym)))
                 (wrap-cps-with-heap0 (cdr func-names0) `(:HEAP ,heap-list (,closure-name) (,expr0)))))))
  (wrap-cps-with-heap0 (reverse func-names) expr)))

;----------------------------------------------------------------
(def-cps-func cps-fixh ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '()))
          (heap-closure-sym (cps-gensym parser))
          (func-names (mapcar #'(lambda (x) (car x)) binds)))

      (cps-parse finder expr finder-env)
      (let* ((free-variables
               (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) (car finder-env))))
             (fixh-free-vars (cons `(:fixh ,func-names) free-variables))
             (new-env (make-new-env parser env free-variables))
             (new-binds (mapcar #'(lambda (bind) (cps-bind-fixh parser bind new-env)) binds))

             (new-next-cps (cps-parse parser next-cps new-env))
             (new-expr `(:FIXH ,new-binds ,new-next-cps))
             (wrapped-cps (wrap-cps-with-heap func-names heap-closure-sym new-expr)))

            `(:HEAP ,free-variables (,heap-closure-sym) (,wrapped-cps))))))

