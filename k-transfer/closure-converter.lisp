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
#|
(defun wrap-cps-with-heap (heap-sym new-next-cps new-env)
  (let ((free-vars (car new-env))
                 (let* ((closure-name (car func-names0))
                        (label0 `(:LABEL ,(make-new-func-name closure-name)))
                        (stack-list (cons label0 
                                          (copy-list free-vars))))
           `(:HEAP ,free-vars (,heap-sym) (,closure-name) (,new-next-cps))))))
|#

;----------------------------------------------------------------
(def-cps-func cps-app ((parser closure-converter) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))

    (let ((closure-name func-name)
          (label0 `(label ,(make-new-func-name func-name)))
          (sym0 (cps-gensym parser)))

      `(:RECORD-REF (,closure-name 0) (,sym0) ((:APP ,sym0 (,closure-name ,@args)))))))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser closure-converter) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (let ((new-func-name (make-new-func-name func-name))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,new-func-name ,args ,(wrap-cps-with-record-ref func-name new-next-cps env)))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixs ((parser closure-converter) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (free-vars (env-to-free-variables env)))

    (let ((wrapped-cps `(,func-name ,args (:POP (,(+ (length free-vars) 1)) () (,next-cps)))))
      (cps-bind parser wrapped-cps env))))

;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '())))
      (cps-parse finder expr finder-env)
      (let* ((free-variables
              (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) (car finder-env))))
            (new-env (make-new-env parser env (cons :fixs free-variables))))

        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind new-env)) binds))
              (new-next-cps (cps-parse parser next-cps new-env))
              (func-names (mapcar #'(lambda (x) (car x)) binds)))

          (let ((wrapped-cps (wrap-cps-with-stack func-names new-next-cps new-env)))
            `(,fix-op ,new-binds ,wrapped-cps)))))))

;----------------------------------------------------------------
#|
(def-cps-func cps-fixh ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (new-env (make-new-env parser env))
        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '()))
          (heap-closure-sym (cps-gensym)))

      (cps-parse finder expr finder-env)
      (let ((free-variables
              (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) (car finder-env)))))
        (setf (car new-env) (cons `(:fixh ,heap-closure-sym) free-variables))

        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind new-env)) binds))
              (new-next-cps (cps-parse parser next-cps new-env))
              (func-names (mapcar #'(lambda (x) (car x)) binds)))

          (let ((wrapped-cps (wrap-cps-with-heap func-names new-next-cps new-env)))
            `(,fix-op ,new-binds ,wrapped-cps))))))
  |#
