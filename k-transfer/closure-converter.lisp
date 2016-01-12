(load "free-variable-finder.lisp")

;----------------------------------------------------------------
(defclass closure-converter (cps-parser)
  ())

;----------------------------------------------------------------
(defun get-free-variables (finder cps-func expr)
  (let ((finder-env (make-new-env finder '())))
    (funcall cps-func finder expr finder-env)
    (let* ((all-variables (car finder-env))
           (free-variables (filter-free-variables all-variables)))
      free-variables)))

;----------------------------------------------------------------
;(def-cps-func cps-bind ((parser closure-converter) expr env))

;----------------------------------------------------------------
(defun make-new-func-name (old-func-name)
  (intern (format nil ":~a" old-func-name)))

;----------------------------------------------------------------
;(((:fixh . closure-sym) v0 v1 v2 ....) ...)
; fixh-free-vars -> ((v1 . ((:fixh closure-sym0) v0 v1 v2 v3))
(defun env-to-free-variables (env)
  (cdar env))

;----------------------------------------------------------------
; env -> (((:fixh . closure-sym) v0 v2 ....) ...)
;          -> ((v3 . (:fixh . closure-sym-0) ... v3 ...)
;              (v4 . (:fixh . closure-sym-1) ... v4 ...))
(defun make-upper-free-vars-list (upper-free-vars env)
  (labels ((make-upper-free-vars-list0 (upper-free-vars0 env0 rv)
              (if (null upper-free-vars0) rv
                (let* ((top-env (car env0))
                       (info (car top-env))
                       (info-id (car info))
                       (free-vars-in-env (cdr top-env))
                       (hit-vars (intersection free-variables0 free-vars-in-env)))
                  (make-upper-free-vars-list0
                    (set-difference free-variables0 free-vars-in-env)
                    (cdr env0)
                    (append rv 
                            (mapcar #'(lambda (x) `(,x . ,top-env)) hit-vars)))))))

    (make-upper-free-vars-list0 upper-free-vars (cdr env) '())))

;----------------------------------------------------------------
; free-vars -> (v0 v1 v2)
; env -> (((:fixh . closure-sym) v0 v2 ....) ...)
; result -> (v1)
(defun get-strict-free-variables (x-free-vars env)

  (labels ((get-strict-free-variables0 (strict-free-vars env0)
              (if (null env0) strict-free-vars
                (let* ((top-env (car env0))
                       (free-vars-in-env (cdr top-env)))

                  (get-strict-free-variables0
                    (set-difference strict-free-vars free-vars-in-env)
                    (cdr env0))))))
    (get-strict-free-variables0 x-free-vars env)))

;----------------------------------------------------------------
;(def-cps-func cps-symbol ((parser closure-converter) sym env)
;              ;todo
;  (labels ((variables-to-record-ref0 (env0)
;              (if (null env0) sym
;                  ;(cps-error-exit parser sym env)
;                (let* ((top-env (car env0))
;                       (info (car top-env))
;                       (info-id (car info)))
;                  (if (member sym top-env)
;                    (if (eq info-id :fixh)
;                      `(,sym . ,top-env)
;                      sym)
;                    (variables-to-record-ref0 (cdr env0)))))))
;    (variables-to-record-ref0 env)))

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
(defun wrap-cps-bind-fixs-with-record-ref (closure-name new-next-cps new-env)
    (labels ((do-wrap (var-names0 no next-cps0)
               (if (null var-names0) next-cps0
                 (let ((var-name (car var-names0)))
                   (wrap-cps-with-record-ref0 (cdr var-names0) (+ no 1) 
                     `(:RECORD-REF (,closure-name ,no) (,var-name) ((,next-cps0))))))))
      (let ((free-vars (env-to-free-variables new-env)))
        (do-wrap free-vars 1 new-next-cps))))

;----------------------------------------------------------------
; tabun iranai ryos todo
;(defun wrap-record-ref (fixh-free-vars next-cps)
;    (labels ((get-num0 (sym vars n)
;               (if (null vars) :NOT-FOUND 
;                (if (eq sym (car vars)) n
;                  (get-num0 sym (cdr vars) (+ n 1)))))
;
;             (wrap-record-ref0 (closure-name sym vars next-cps0)
;                (let ((no (get-num0 sym vars 0)))
;                 `(:RECORD-REF (,closure-name ,no) (,sym) ((,next-cps0)))))
;
;             (wrap-record-ref1 (fixh-free-vars1 next-cps1)
;                (if (null fixh-free-vars1) next-cps1
;                  (let* ((elm (car fixh-free-vars1))
;                         (sym (car elm))
;                         (vars (cdr elm))
;                         (closure-name (cdar vars)))
;
;                    (wrap-record-ref1
;                      (cdr fixh-free-vars1)
;                      (wrap-record-ref0 closure-name sym vars next-cps1))))))
;
;      (wrap-record-ref1 fixh-free-vars next-cps)))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser closure-converter) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))

    (let ((closure-name func-name)
          (sym0 (cps-gensym parser)))
      (copy-tree `(:RECORD-REF ,(list closure-name 0) (,sym0) ((:APP ,sym0 (,closure-name ,@args))))))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixs ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (free-vars (env-to-free-variables env)))

    (let* ((func-name (make-new-func-name closure-name))
           (new-args (cons closure-name args))

           (new-id `((:primitive . fixs-bind) ,@args))
           (new-env (make-new-env parser env new-id))
           (new-next-cps (cps-parse parser next-cps new-env))
           (pop-cps `(:POP (,(+ (length free-vars) 1)) () (,new-next-cps)))
           (wrapped-cps (wrap-cps-bind-fixs-with-record-ref closure-name pop-cps env)))

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

      (let* ((all-variables (car finder-env))
             (free-variables (filter-free-variables all-variables))
             (strict-free-vars
               (get-strict-free-variables free-variables env))

             (fixs-free-vars `((:fixs) ,@free-variables))
             (new-env (make-new-env parser env fixs-free-vars)))

        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind new-env)) binds))
              (new-next-cps (cps-parse parser next-cps new-env))
              (func-names (mapcar #'(lambda (x) (car x)) binds)))

          (let ((wrapped-cps (wrap-cps-with-stack func-names new-next-cps new-env)))
            `(,fix-op ,new-binds ,wrapped-cps)))))))

;----------------------------------------------------------------
; top-env -> ((:fixh . closure-sym) v0 v1 v2 ....
;                          (v3 . ((:fixh . closure-sym-0)  ... v3 ... ))
;                          (v4 . ((:fixh . closure-sym-0)  ... v4 ... ))
;                          (v5 . ((:fixh . closure-sym-0)  ... v5 ... )))
;           or :fixs :primitive
;
(defun wrap-cps-bind-fixh-with-record-ref (parser this-free-vars new-next-cps top-env)
  (let ((closure-name (cdar top-env)))
    (labels ((get-num0 (sym vars n)
               (if (null vars) :NOT-FOUND 
                 (let ((elm (car vars)))
                   (if (eq sym elm) n
                     (if (and (listp elm) (eq sym (car elm)))
                       (cdr elm)
                       (get-num0 sym (cdr vars) (+ n 1)))))))

             (do-wrap0 (sym cps-expr0)
                (let ((n-info (get-num0 sym top-env 0)))
                  (if (atom n-info)
                    `(:RECORD-REF (,closure-name ,n-info) (,sym) (,cps-expr0))

                    (let ((new-sym (cps-gensym parser))
                          (uppper-closure-name (caar n-info)) ; not used
                          (nexted-no (position sym n-info)))
                      `(:RECORD-REF (,closure-name ,no) (,new-sym) (
                          `(:RECORD-REF (,new-sym ,nexted-no) (,sym) (,cps-expr0))))))))

             (do-wrap1 (free-vars1 cps-expr1)
                (if (null free-vars1) cps-expr1
                  (let ((sym (car free-vars1)))
                    (do-wrap1
                      (cdr free-vars1)
                           (do-wrap0 sym cps-expr1))))))

    (do-wrap1 this-free-vars new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixh ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (finder (make-instance 'free-variable-finder)))

    ;(print `(cps-bind-fixh ,free-vars))
    (let ((finder-env (make-new-env finder '())))

      (cps-bind finder expr finder-env)

      (let* ((all-variables (car finder-env))
             (free-variables (filter-free-variables all-variables))

             (func-name (make-new-func-name closure-name))
             (new-args (cons closure-name args))
             (new-id `((:primitive . fixs-bind) ,@args))
             (new-env (make-new-env parser env new-id))
             (new-next-cps (copy-tree (cps-parse parser next-cps new-env)))

             (wrapped-cps (wrap-cps-bind-fixh-with-record-ref parser free-variables new-next-cps (car env)))) ; use env , it's ok

          `(,func-name ,new-args ,wrapped-cps)))))

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
          (func-names (mapcar #'(lambda (x) (car x)) binds)))

      ;(mapc #'(lambda (bind) (cps-bind finder bind finder-env)) binds)
      (cps-binds finder binds finder-env)

      (let* ((all-variables (car finder-env))
             (func-names-is-1? (= (length func-names) 1))
             (heap-closure-sym (if func-names-is-1?
                                 (car func-names)
                                 (cps-gensym parser)))
             (free-variables (filter-free-variables all-variables))
             (strict-free-vars
               (get-strict-free-variables free-variables env))

             (upper-free-vars-list
               (make-upper-free-vars-list (set-difference free-variables strict-free-vars) env))

             (fixh-free-vars `((:fixh . ,heap-closure-sym) ,@strict-free-vars ,@upper-free-vars-list))

             (new-env (make-new-env parser env fixh-free-vars))
             (new-binds (mapcar #'(lambda (bind) (cps-bind-fixh parser bind new-env)) binds))

             (new-next-cps (cps-parse parser next-cps new-env))

             (upper-closure-list
                 (mapcar #'(lambda (upper-vars) (cdadr upper-vars)) upper-free-vars-list))
             (closure-list `(,@(copy-list strict-free-vars) ,@upper-closure-list))
             (label0 (if func-names-is-1?
               `(:LABEL ,(make-new-func-name heap-closure-sym))
               `(:LABEL :DUMMY)))

             (heap-list `(,label0 ,@closure-list))
             (heap-expr (if func-names-is-1?
                new-next-cps
                (wrap-cps-with-heap func-names heap-closure-sym new-next-cps))))

        `(:FIXH ,new-binds ,
                `(:HEAP ,heap-list (,heap-closure-sym) (,heap-expr)))))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser closure-converter) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (let* ((new-id `((:primitive . ,op) ,@args))
           (new-env (make-new-env parser env new-id))
           (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps new-env)) next-cpss)))

      `(,op ,args ,result ,new-next-cpss))))
