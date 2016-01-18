(load "free-variable-finder.lisp")

;----------------------------------------------------------------
(defclass closure-converter (cps-parser)
  ())

;----------------------------------------------------------------
(defun make-new-func-name (old-func-name)
  (intern (format nil ":~a" old-func-name)))

;----------------------------------------------------------------
; FIXS
;----------------------------------------------------------------
;----------------------------------------------------------------
;((:fixs . closure-sym) v0 v1 v2 ....) ...)
;
;
(defun env-to-free-variables-fixs (env)
  (cdar env))

;----------------------------------------------------------------
(defun wrap-cps-with-stack (func-names new-next-cps new-env)
  (let ((free-vars (env-to-free-variables-fixs new-env)))
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
                   (do-wrap (cdr var-names0) (+ no 1) 
                     `(:RECORD-REF (,closure-name ,no) (,var-name) (,next-cps0)))))))
      (let ((free-vars (env-to-free-variables-fixs new-env)))
        (do-wrap free-vars 1 new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixs ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (free-vars (env-to-free-variables-fixs env)))

    (let* ((func-name (make-new-func-name closure-name))
           (new-args (cons closure-name args))

           (new-next-cps (cps-parse parser next-cps env))
           (free-vars-len (length free-vars))
           ;(x (print `(,free-vars ,free-vars-len)))
           (wrapped-cps (if (= free-vars-len 0)
                          new-next-cps
                          (let ((pop-cps `(:POP (,(+ free-vars-len 1)) () (,new-next-cps))))
                            (wrap-cps-bind-fixs-with-record-ref closure-name pop-cps env)))))

             `(,func-name ,new-args ,wrapped-cps))))

;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder :suppress-cps-fixs t)))

    (let ((finder-env (make-new-env finder '()))
          (func-names (mapcar #'(lambda (x) (car x)) binds)))

      (cps-binds finder binds finder-env)

      (let* ((all-variables (car finder-env))
             (free-variables (filter-free-variables all-variables))
             ;(x (print `(var ,all-variables :free ,free-variables)))

             (fixs-free-vars `((:fixs) ,@free-variables))
             (env-binds (make-new-env parser env fixs-free-vars)))
             
        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind env-binds)) binds))
              (new-next-cps (cps-parse parser next-cps env)))

            (let ((wrapped-cps 
                    (if (null free-variables) new-next-cps
                      (wrap-cps-with-stack func-names new-next-cps env-binds))))
              `(,fix-op ,new-binds ,wrapped-cps)))))))

;----------------------------------------------------------------
; FIXH
;----------------------------------------------------------------
;----------------------------------------------------------------
; free-vars -> (v0 v1 v2)
; env -> (((:fixh . closure-sym) v0 v2 ....) ...)
; result -> (v1)
(defun get-strict-free-variables (x-free-vars env)

  (labels ((get-strict-free-variables0 (strict-free-vars env0)
              (if (null env0) strict-free-vars
                (let* ((top-env (car env0))
                       (key-word (caar top-env))
                       (free-vars-in-env (cdr top-env)))

                    (get-strict-free-variables0
                      (if (eq key-word :fixh)
                        (set-difference strict-free-vars free-vars-in-env)
                        strict-free-vars)
                      (cdr env0))))))

    (get-strict-free-variables0 x-free-vars env)))

;----------------------------------------------------------------
; env -> (((:fixh . closure-sym) v0 v2 ....) ...)
;          -> ((v3 . (:fixh . closure-sym-0) ... v3 ...)
;              (v4 . (:fixh . closure-sym-1) ... v4 ...))
;
; only search :fixh free variables

(defun make-upper-free-vars-list (upper-free-vars env)
  (labels ((make-upper-free-vars-list0 (free-variables0 env0 rv)
              (if (null free-variables0) rv
                (let* ((top-env (car env0))
                       (info (car top-env))
                       (info-id (car info))
                       (free-vars-in-env 
                         (if (eq info-id :fixh)
                                 (cdr top-env)
                                 '()))
                       (hit-vars (intersection free-variables0 free-vars-in-env)))


                  (make-upper-free-vars-list0
                    (set-difference free-variables0 free-vars-in-env)
                    (cdr env0)
                    (append rv 
                            (mapcar #'(lambda (x) `(,x . ,top-env)) hit-vars)))))))

    (make-upper-free-vars-list0 upper-free-vars env '())))

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
; top-env -> ((:fixh . closure-sym) v0 v1 v2 ....
;                          (v3 . ((:fixh . closure-sym-0)  ... v3 ... ))
;                          (v4 . ((:fixh . closure-sym-0)  ... v4 ... ))
;                          (v5 . ((:fixh . closure-sym-0)  ... v5 ... )))
;           or :fixs :primitive
;
(defun wrap-cps-bind-fixh-with-record-ref (parser this-free-vars new-next-cps top-env)
  ;(print `(top-env ,top-env))
  (let ((closure-name (cdar top-env))
        (new-symbol-pos-pairs '()))

    (labels ((get-pressed-num0 (sym vars n)
              (if (null vars) :NOT-FOUND 
                 (let ((elm (car vars)))
                   ;(print `(eleelm ,elm ,(cdr elm)))
                     (if (and (listp elm) (listp (cdr elm)) (eq sym (cdadr elm)))
                       n
                       (get-pressed-num0 sym (cdr vars) (+ n 1))))))

             (get-num0 (sym vars n)
               (if (null vars) :NOT-FOUND 
                 (let ((elm (car vars)))
                   (if (eq sym elm) (values n :FOUND-THE-SYM)
                     (if (and (listp elm) (eq sym (car elm)))
                       ;(values n (cdr elm))
                       (values 
                         (get-pressed-num0 (cdadr elm) top-env 0)
                               (cdr elm))
                       (get-num0 sym (cdr vars) (+ n 1)))))))

             (do-wrap0 (sym cps-expr0)
                (multiple-value-bind (no n-info) (get-num0 sym top-env 0)
                  ;(print `(do-wrap0 ,sym ,no ,n-info))
                  (if (atom n-info)
                    `(:RECORD-REF (,closure-name ,no) (,sym) (,cps-expr0))

                    (let* ((nexted-no (position sym n-info))
                           (base-sym (cdar n-info))
                           (stock-sym (cddr (assoc base-sym new-symbol-pos-pairs)))
                           (ref-sym 
                             (if stock-sym stock-sym (cps-gensym parser)))
                           (inner-cps-expr0 `(:RECORD-REF (,ref-sym ,nexted-no) (,sym) (,cps-expr0))))
                      ;(print `(stock-sym ,sym ,(copy-tree stock-sym) ,new-symbol-pos-pairs))
                      (if (null stock-sym)
                        (setf new-symbol-pos-pairs (cons `(,base-sym . (,no . ,ref-sym)) new-symbol-pos-pairs)))
                      inner-cps-expr0))))

             (do-wrap1 (free-vars1 cps-expr1)
                (if (null free-vars1) cps-expr1
                  (let ((sym (car free-vars1)))
                    (do-wrap1
                      (cdr free-vars1)
                           (do-wrap0 sym cps-expr1)))))

             (do-wrap2 (cps-expr2 new-symbol-pos-pairs0)
                (if (null new-symbol-pos-pairs0) cps-expr2
                  (let* ((kv (car new-symbol-pos-pairs0))
                         (kv-kv (cdr kv))
                         (no (car kv-kv))
                         (ref-sym (cdr kv-kv)))
                    (do-wrap2 
                      `(:RECORD-REF (,closure-name ,no) (,ref-sym) (,cps-expr2)) (cdr new-symbol-pos-pairs0))))))

      ;(print `(this-free-vars ,closure-name ,this-free-vars ,(null this-free-vars)))
      (do-wrap2 
        (do-wrap1 this-free-vars new-next-cps) new-symbol-pos-pairs))))

;----------------------------------------------------------------
(defun copy-env-with-new-sym (parser env)
  (let ((new-env (copy-tree env)))
    (setf (cdaar new-env) (cps-gensym parser))
    new-env))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixh ((parser closure-converter) expr env0)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (env-closure-name (cdaar env0))

        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '())))

      (cps-bind finder expr finder-env)

      (let* ((make-new-sym? (eq env-closure-name :make-new-sym))
             (env (if make-new-sym? (copy-env-with-new-sym parser env0) env0))
             (all-variables (car finder-env))
             (free-variables (filter-free-variables all-variables))
             ;(z (print `(free-variables ,closure-name ,free-variables)))

             (func-name (make-new-func-name closure-name))
             (new-args (cons closure-name args))

             (new-next-cps (copy-tree (cps-parse parser next-cps env)))

             (wrapped-cps (wrap-cps-bind-fixh-with-record-ref parser free-variables new-next-cps (car env))) ; use env , it's ok
             (new-wrapped-cps (if (and make-new-sym? (not (null free-variables)))
                                (copy-tree `(:RECORD-REF (,closure-name 1) (,(cdaar env)) (,wrapped-cps)))
                                wrapped-cps))) 

          `(,func-name ,new-args ,new-wrapped-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-fixh ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '()))
          (func-names (mapcar #'(lambda (x) (car x)) binds)))

      (cps-binds finder binds finder-env)

      (let* ((all-variables (car finder-env))
             ;(x (print `(all-variables ,all-variables)))
             (func-names-is-1? (= (length func-names) 1))
             (env-closure-sym (if func-names-is-1?
                                 (car func-names)
                                 :make-new-sym))
             (heap-closure-sym (if func-names-is-1?
                                 (car func-names)
                                 (cps-gensym parser)))
             (free-variables (filter-free-variables all-variables))
             (strict-free-vars
               (get-strict-free-variables free-variables (cdr env)))

             (upper-free-vars-list
               (make-upper-free-vars-list (set-difference free-variables strict-free-vars) env))
             ;(x (print `(fix-hs ,(copy-tree free-variables) :S ,(copy-tree strict-free-vars) :U ,(copy-tree upper-free-vars-list) )))

             (fixh-free-vars `((:fixh . ,env-closure-sym) ,@strict-free-vars ,@upper-free-vars-list))

             (new-env (make-new-env parser env fixh-free-vars))

             (new-binds (mapcar #'(lambda (bind) (cps-bind-fixh parser bind new-env)) binds))

             (new-next-cps (cps-parse parser next-cps env))

             (upper-closure-list
                 (cddr (mapcar #'(lambda (upper-vars) (cdadr upper-vars)) upper-free-vars-list)))
             (closure-list `(,@(copy-list strict-free-vars) ,@upper-closure-list))
             (label0 (if func-names-is-1?
               `(:LABEL ,(make-new-func-name heap-closure-sym))
               (copy-list '(:LABEL :DUMMY))))

             (heap-list `(,label0 ,@closure-list))
             (heap-expr (if func-names-is-1?
                new-next-cps
                (wrap-cps-with-heap func-names heap-closure-sym new-next-cps))))

        `(:FIXH ,new-binds ,
                `(:HEAP ,heap-list (,heap-closure-sym) (,heap-expr)))))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser closure-converter) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))

    (let ((closure-name func-name)
          (sym0 (cps-gensym parser)))
      (copy-tree `(:RECORD-REF ,(list closure-name 0) (,sym0) ((:APP ,sym0 (,closure-name ,@args))))))))

