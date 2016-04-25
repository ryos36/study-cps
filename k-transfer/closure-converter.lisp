;----------------------------------------------------------------
(in-package :sinby.cps.closure-converter)

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
(defun wrap-cps-with-stack (func-names new-next-cps free-vars)
  (labels ((wrap-cps-with-stack0 (func-names0 next-cps0)
             (if (null func-names0) next-cps0
               (let* ((closure-name (car func-names0))
                      (label0 `(:LABEL ,(make-new-func-name closure-name)))
                      (stack-list (cons label0 
                                        (copy-list free-vars))))
                 (wrap-cps-with-stack0 (cdr func-names0) `(:STACK ,stack-list (,closure-name) (,new-next-cps)))))))
    (wrap-cps-with-stack0 (reverse func-names) new-next-cps)))

;----------------------------------------------------------------
(defun wrap-cps-bind-fixs-with-record-ref (closure-name new-next-cps free-vars)
    (labels ((do-wrap (var-names0 no next-cps0)
               (if (null var-names0) next-cps0
                 (let ((var-name (car var-names0)))
                   (do-wrap (cdr var-names0) (+ no 1) 
                     `(:RECORD-REF (,closure-name ,no) (,var-name) (,next-cps0)))))))
      (do-wrap free-vars 1 new-next-cps)))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixs ((parser closure-converter) expr env)
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (pure-free-vars (env-to-free-variables-fixs env)))

    (let* ((func-name (make-new-func-name closure-name))
           (new-args (cons closure-name args))

           (new-id `((:primitive . :fixs-bind) ,func-name ,@args))
           (new-env (make-new-env parser env new-id))

           (new-next-cps (cps-parse parser next-cps new-env))
           (pure-free-vars-len (length pure-free-vars))
           ;(x (print `(,pure-free-vars ,pure-free-vars-len)))
           (wrapped-cps (if (= pure-free-vars-len 0)
                          new-next-cps
                          (let ((pop-cps `(:POP (,(+ pure-free-vars-len 1)) () (,new-next-cps))))
                            (wrap-cps-bind-fixs-with-record-ref closure-name pop-cps pure-free-vars)))))

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
             ;(x (print `(:func-names ,func-names :var ,all-variables :free ,free-variables)))

             (fixs-free-vars `((:fixs) ,@free-variables))
             (env-binds (make-new-env parser env fixs-free-vars))

             (new-id `((:primitive . :fixs-funcs) ,@func-names))
             (env-next-cps (make-new-env parser env new-id)))
             
        (let ((new-binds (mapcar #'(lambda (bind) (cps-bind-fixs parser bind env-binds)) binds))
              (new-next-cps (cps-parse parser next-cps env-next-cps)))

            (let ((wrapped-cps 
                    (if (null free-variables) new-next-cps
                      (wrap-cps-with-stack func-names new-next-cps free-variables))))
              `(,fix-op ,new-binds ,wrapped-cps)))))))

;----------------------------------------------------------------
; FIXH
;----------------------------------------------------------------
;----------------------------------------------------------------
; env -> (((:fixh . closure-name) v0 v2 ....)
;          ....
;          ((:primitive . :define) g0)
;          ....
;          ((:primitive . :define) g1))
; result -> (g0 g1 g2)

(defun get-global-variables (env)
  (nreverse
    (remove-if #'null 
               (mapcar #'(lambda (x)
                           (if (and (eq (caar x) :primitive)
                                    (eq (cdar x) :define))
                             (cadr x))) env ))))

; (g0 g1 g2) -> ((g0 (:global-closure . :global-variable-pointer) g0 g1 g2)
;                (g1 (:global-closure . :global-variable-pointer) g0 g1 g2)
;                (g2 (:global-closure . :global-variable-pointer) g0 g1 g2))
;
(defun global-vars->list (gvars)
  (mapcar #'(lambda (x)
              `(,x (:global-closure . :global-variable-pointer) ,@gvars)) gvars))

;----------------------------------------------------------------
; free-vars -> (v0 v1 v2)
; env -> (((:fixh . closure-name) v0 v2 ....) ...)
; result -> (v1)
(defun get-strict-free-variables (free-vars env)

  (labels ((get-strict-free-variables0 (strict-free-vars0 env0 saved-vars)
              (if (null env0) (append strict-free-vars0 saved-vars)
                (let* ((top-env (car env0))
                       (key-word (caar top-env))
                       (free-vars-in-env (cdr top-env)))

                  (if (not (eq key-word :fixh))
                    (setf saved-vars
                          (append saved-vars
                                  (intersection strict-free-vars0 free-vars-in-env))))

                  ;(print `(get-strict-free-variables0 (,strict-free-vars0 ,top-env)))
                  (get-strict-free-variables0
                    (set-difference strict-free-vars0 free-vars-in-env)
                    (cdr env0)
                    saved-vars)))))

    (let ((strict-free-vars                            
            (get-strict-free-variables0 free-vars env '())))

      ; keep order
      (remove-if #'null (mapcar #'(lambda (x) (car (member x strict-free-vars))) free-vars)))))

;----------------------------------------------------------------
; env -> (((:fixh . closure-name) v0 v2 ....) ...)
;          -> ((v3 (:fixh . closure-name) ... v3 ...)
;              (v4 (:fixh . closure-name) ... v4 ...))
;
; only search :fixh free variables

(defun make-upper-free-vars-list (upper-free-vars global-vars env)
  (labels ((make-upper-free-vars-list0 (free-variables0 env0 rv)
              (if (null env0) (copy-tree rv)
                (let* ((top-env (car env0))
                       (info (car top-env))
                       (info-id (car info))
                       (free-vars-in-env (cdr top-env))
                       (free-vars-in-fixh
                         (if (eq info-id :fixh) free-vars-in-env '()))
                       (fixh-hit-vars (intersection free-variables0 free-vars-in-fixh)))

                  ;(print `(:xxx ,fixh-hit-vars ,top-env (,fixh-hit-vars . ,top-env)))
                  (make-upper-free-vars-list0
                    (set-difference free-variables0 free-vars-in-env)
                    (cdr env0)
                    (append rv 
                            (mapcar #'(lambda (x) `(,x . ,top-env)) fixh-hit-vars)))))))

    ;(print `(:env ,upper-free-vars ,env))

    (make-upper-free-vars-list0 (set-difference upper-free-vars global-vars) env '())))

;----------------------------------------------------------------
; remove same name
(defun filter-upper-closure-list (upper-closure-list)
    (labels ((filter-upper-closure-list0 (upper-closure-list0 already-used-var)
                (if (null upper-closure-list0) (nreverse already-used-var)
                  (let ((sym (car upper-closure-list0)))
                    (if (null (find sym already-used-var))
                      (push sym already-used-var))
                    (filter-upper-closure-list0 (cdr upper-closure-list0) already-used-var)))))
      (filter-upper-closure-list0 upper-closure-list '())))

;----------------------------------------------------------------
; top-env -> ((:fixh . closure-name) (:fixh . closure-name) ... v0 v1 v2 ....
;                          (v3 . ((:fixh . closure-name) .. v3 ... ))
;                          (v4 . ((:fixh . closure-name) .. v4 ... ))
;                          (v5 . ((:fixh . closure-name) .. v5 ... ))
;                          (g6 . ((:global-closure :global-closure) ... g6 ...))
;
;           or :fixs :primitive
; Note: (v3 . ((:fixh . closure-name) ... v3 ))
;         => (v3 (:fixh . closure-name) ... v3 ) 
;
(defun wrap-cps-bind-fixh-with-record-ref (parser this-free-vars new-next-cps top-env)
  ;(print `(:top-env ,top-env))
  (let ((closure-name (cdar top-env))
        (new-symbol-pos-pairs '()))
  ;(print `(:wrap-cps-bind-fixh-with-record-ref ,closure-name))
  ;(print `(:wrap-cps-bind-fixh-with-record-ref ,this-free-vars, new-next-cps))

    (labels ((get-pressed-num0 (sym vars n)
              ;(print `(:pressed ,sym ,vars ,n))
              (if (null vars) :NOT-FOUND
                 (let ((elm (car vars)))
                   ;(print `(:eleelm ,elm ,(if (consp elm) (cdr elm))))
                     (if (and (listp elm) (listp (cdr elm)) (eq sym (cdadr elm)))
                       n
                       (get-pressed-num0 sym (cdr vars) (+ n 1))))))

             (get-num0 (sym vars n)
               (if (null vars) :NOT-FOUND 
                 (let ((elm (car vars)))
                   (if (eq sym elm) (values n :FOUND-THE-SYM)
                     (if (and (listp elm) (eq sym (car elm)))
                       ;(values n (cdr elm))
                       (let ((closure-name (cdadr elm)))
                         (if (eq closure-name :global-variable-pointer)
                           (let ((gvars (cddr elm)))
                             (values
                               (position sym gvars)
                               (cadr elm)))
                           (values 
                               (get-pressed-num0 closure-name top-env 0)
                               (cdr elm))))
                       (get-num0 sym (cdr vars) (+ n 1)))))))

             (get-off-num (sym vars n)
               ;(print `(:get-off-num ,sym ,vars ,n))
               (assert vars)
               (let ((check-fixh-tagged-list (car vars)))
                 (if (not (and (consp check-fixh-tagged-list)
                               (eq :fixh (car check-fixh-tagged-list))))
                   (get-num0 sym vars n)

                   ;found (fixh . closure-name)
                   (let ((k-sym (cdr check-fixh-tagged-list)))
                     (if (eq k-sym sym)
                       (values n check-fixh-tagged-list)
                       (get-off-num sym (cdr vars) (+ n 1)))))))


             (do-wrap0 (sym cps-expr0)
                (multiple-value-bind (no n-info) (get-off-num sym top-env 0)
                  ;(print `(:do-wrap0 ,sym ,no ,n-info :key ,(if (consp n-info) (car n-info))))
                  (assert (not (eq no :NOT-FOUND)))
                  ;(print `(,no :+ ,func-pos :=> :new-no))
                  (if (atom n-info)
                    `(:RECORD-REF (,closure-name ,no) (,sym) (,cps-expr0))

                    (case (car n-info)
                      (:global-closure
                      `(:RECORD-REF (:GLOBAL-VARIABLE-POINTER ,no) (,sym) (,cps-expr0)))
                      (:fixh 
                        `(:RECORD-OFFS (,closure-name ,no) (,sym) (,cps-expr0)))

                      (otherwise
                        (let* ((nexted-no (position sym n-info))
                               ;(x (print `(:caddar-n-info ,n-info)))
                               (base-sym (cdar n-info))
                               (stock-sym (cddr (assoc base-sym new-symbol-pos-pairs)))
                               (ref-sym 
                                 (if stock-sym stock-sym (cps-gensym parser)))
                               (inner-cps-expr0 `(:RECORD-REF (,ref-sym ,nexted-no) (,sym) (,cps-expr0))))
                          ;(print `(stock-sym ,sym ,(copy-tree stock-sym) ,new-symbol-pos-pairs))
                          (if (null stock-sym)
                            (push `(,base-sym . (,no . ,ref-sym)) new-symbol-pos-pairs))
                          inner-cps-expr0))))))

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
                         (new-no (car kv-kv))
                         (ref-sym (cdr kv-kv)))
                    (do-wrap2 
                      `(:RECORD-REF (,closure-name ,new-no) (,ref-sym) (,cps-expr2)) (cdr new-symbol-pos-pairs0))))))

      ;(print `(:this-free-vars ,closure-name ,this-free-vars ,(null this-free-vars)))
      (do-wrap2 
        (do-wrap1 this-free-vars new-next-cps) (nreverse new-symbol-pos-pairs)))))

;----------------------------------------------------------------
(def-cps-func cps-bind-fixh ((parser closure-converter) expr env)
  ;(print `(:cps-bind-fixh ,expr :env ,env))
  (let ((closure-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (finder (make-instance 'free-variable-finder)))

    (let ((finder-env (make-new-env finder '())))

      (cps-bind finder expr finder-env)

      (let* ((free-vars-env (car env))
             (all-variables (car finder-env))
             (free-variables (filter-free-variables all-variables))
             ;(z (print `(:free-variables ,closure-name ,free-variables)))

             (func-name (make-new-func-name closure-name))
             (new-args (cons closure-name args))

             (new-id `((:primitive . :fixh-bind) ,func-name ,@args))
             (new-env (make-new-env parser env new-id))

             (new-next-cps (copy-tree (cps-parse parser next-cps new-env)))

             (wrapped-cps (wrap-cps-bind-fixh-with-record-ref parser free-variables new-next-cps free-vars-env))) 

          `(,func-name ,new-args ,wrapped-cps)))))

;----------------------------------------------------------------
    ;(v0 . t)
    ;(v1) v1 is free
(defmethod get-ordered-func-name ((parser closure-converter) r-func-names r-env-list)
  (let (ref-vars)
    (labels ((get-cmp-n (remain)
                        (- 
                          (reduce #'(lambda (r x) (max r (length x))) 
                                  remain
                                  :initial-value 0) 1)) 
             (get-ordered-func-name0 (r-free-vars-list0 rv remain)
               ;(print `(:get-name0 ,r-free-vars-list0 :rv ,rv))
               (if (null r-free-vars-list0) (values rv
                                                    (nreverse remain))

                 (let* ((top-elm (car r-free-vars-list0))
                        (func-name (car top-elm))
                        (ref-vars-list (set-difference (cdr top-elm) rv)))

                   (if (null ref-vars-list)
                     (push func-name rv)
                     (push (cons func-name ref-vars-list) remain))
                   (get-ordered-func-name0 (cdr r-free-vars-list0) rv remain))))

             (get-ordered-func-name1 (r-free-vars-list1 src-rv old-len)
                  (multiple-value-bind (rv remain) 
                    (get-ordered-func-name0 r-free-vars-list1 src-rv '())

                    (if (null remain)
                      rv

                      (let ((rv-len (length rv)))
                        (if (= old-len rv-len)
                          (get-ordered-func-name-next1 remain rv 0 (get-cmp-n remain))
                          (get-ordered-func-name1 remain rv rv-len))))))

             (get-ordered-func-name-next0 (r-free-vars-list-next0 rv remain cmp-n)

               (if (null r-free-vars-list-next0) (values rv
                                                         (nreverse remain))
                 (let* ((top-elm (car r-free-vars-list-next0))
                        (func-name (car top-elm))
                        (ref-vars-list (set-difference (set-difference (cdr top-elm) rv) ref-vars)))

                   (if (and (null remain) (= (length ref-vars-list) cmp-n))
                     (let ((ref-sym (car ref-vars-list)))
                       (push func-name rv)
                       (mapc #'(lambda (ref-sym)
                                 (push ref-sym ref-vars)) ref-vars-list))

                     (push (cons func-name ref-vars-list) remain))
                   (get-ordered-func-name-next0 (cdr r-free-vars-list-next0) rv remain cmp-n))))

             (get-ordered-func-name-next1 (r-free-vars-list-next1 src-rv old-len cmp-n)
                  (multiple-value-bind (rv remain) 
                    (get-ordered-func-name-next0 r-free-vars-list-next1 src-rv '() cmp-n)

                    (if (null remain)
                      rv

                      (let ((rv-len (length rv)))
                        (if (= old-len rv-len)
                          (get-ordered-func-name-next1 remain rv 0 (get-cmp-n remain))
                          (get-ordered-func-name1 remain rv 0)))))))

      (let* ((r-free-vars-list
               (mapcar #'(lambda (all-variables)
                           (filter-free-variables all-variables)) r-env-list))
             (r-ref-func-list
               (mapcar #'(lambda (vars self-name)
                           (cons self-name
                                 (remove self-name
                                         (intersection vars r-func-names))))
                       r-free-vars-list
                       r-func-names))
             ;(x (print `(:r-ref-func-list ,r-ref-func-list)))
             (rv (get-ordered-func-name1 r-ref-func-list '() 0)))
        (values rv (nreverse ref-vars))))))

;----------------------------------------------------------------
(defun merge-env (r-env-list)
  (reduce #'(lambda (src free-vars) 
                (union src free-vars :test #'equal)) r-env-list))

;----------------------------------------------------------------
(defmethod make-wrapped-record-offs ((parser closure-converter) k-sym func-names off-vars next-cps)
  (labels ((make-wrapped-record-offs0 (off-vars0 next-cps0)
            (if (null off-vars0) next-cps0
              (let* ((sym (car off-vars0))
                     (offset-n (position sym func-names)))
                ;(print `(:off-n ,offset-n ,off-vars :=> ,func-names))
                (assert (not (= 0 offset-n)))
                (make-wrapped-record-offs0 (cdr off-vars0)
                  `(:RECORD-OFFS (,k-sym ,offset-n) (,sym) (,next-cps0)))))))
    (make-wrapped-record-offs0 off-vars next-cps)))

;----------------------------------------------------------------
(defmethod make-wrapped-heap ((parser closure-converter) k-sym ref-vars func-names next-free-funcs next-cps)
  (labels ((make-wrapped-heap0 (ref-vars0 n next-cps0)
            (if (null ref-vars0) next-cps0
              (let* ((sym (car ref-vars0))
                     (sym0 (find sym next-free-funcs))
                     (sym1 (if sym0 sym (cps-gensym parser)))
                     (offset-n (position sym func-names))
                     (record-set!-cps 
                       `(:RECORD-SET! (,sym1 ,n ,k-sym) () (,next-cps0)))
                     (record-off-cps
                       (if sym0 record-set!-cps
                         `(:RECORD-OFFS (,k-sym ,offset-n) (,sym1) (,record-set!-cps)))))
                (make-wrapped-heap0 (cdr ref-vars0) (+ n 1) record-off-cps)))))
    (make-wrapped-heap0 ref-vars (length func-names) next-cps)))

;----------------------------------------------------------------
(def-cps-func cps-fixh ((parser closure-converter) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (finder (make-instance 'free-variable-finder)))

    (let* ((finder-env (make-new-env finder '() '()))
           (next-finder-env (make-new-env finder '() '()))
           (func-names (mapcar #'(lambda (x) (car x)) binds))
           (r-func-names (reverse func-names))
           (r-env-list '()))
          
      (mapc #'(lambda (bind)
                (let ((finder-env (make-new-env finder '() '())))
                  (cps-bind finder bind finder-env)
                  (push (car finder-env) r-env-list))) binds)

      (cps-binds finder binds finder-env)
      ;(print `(:finder-env ,finder-env :env ,env))
      (cps-parse finder next-cps next-finder-env)

      (multiple-value-bind (new-func-names ref-vars)
        (get-ordered-func-name parser r-func-names r-env-list)

        ;(print `(:func-names ,func-names :new-func-names ,(copy-tree new-func-names) :ref-vars ,ref-vars))

        ;(print `(:diff-env :good ,finder-env :new ,(copy-tree (merge-env r-env-list)) :ref ,ref-vars))

        (let* ((all-variables (merge-env r-env-list))
               ;(u (print `(FUNC ,@func-names :env ,env)))
               ;(x (print `(:all-variables ,all-variables)))
               (func-names-is-1? (= (length func-names) 1))
               (free-variables (filter-free-variables all-variables))
               (strict-free-vars
                 (get-strict-free-variables free-variables env))

               (global-vars (get-global-variables env))
               (upper-free-vars-list
                 (make-upper-free-vars-list (set-difference free-variables strict-free-vars) global-vars env))
               ;(x (print `(:fix-hs :F ,(copy-tree free-variables) :S ,(copy-tree strict-free-vars) :D ,(set-difference free-variables strict-free-vars) :U ,(copy-tree upper-free-vars-list) )))
               (fixh-list (mapcar #'(lambda (f) `(:fixh . ,f)) new-func-names))
               ;(x (print `(:fixh-list ,fixh-list)))
               (strict-free-vars-without-func-names
                 (set-difference strict-free-vars (union func-names global-vars)))

               (global-vars-list (global-vars->list global-vars))

               (new-binds (mapcar #'(lambda (bind)
                                      (let* ((func-name (car bind))
                                             (updated-fixh-list
                                               (member func-name fixh-list :test #'(lambda (sym lst) (eq sym (cdr lst)))))
                                             (fixh-free-vars `(,@updated-fixh-list
                                                               ,@ref-vars
                                                               ,@strict-free-vars-without-func-names
                                                               ,@upper-free-vars-list
                                                               ,@global-vars-list
                                                               ))
                                             (env-bind (make-new-env parser env fixh-free-vars)))
                                        (cps-bind-fixh parser bind env-bind))) binds))

               (new-id `((:primitive . :fixh-funcs) ,@func-names))
               (env-next-cps (make-new-env parser env new-id))

               (new-next-cps (cps-parse parser next-cps env-next-cps))

               (upper-closure-list
                   (filter-upper-closure-list (mapcar #'(lambda (upper-vars) (cdadr upper-vars)) upper-free-vars-list)))
               (closure-list (set-difference `(,@(copy-list strict-free-vars) ,@upper-closure-list) global-vars))
               ;(x (print `(:uc ,upper-free-vars-list ,upper-closure-list ,closure-list)))

               (heap-list (append (mapcar #'(lambda (func-name) `(:LABEL ,(make-new-func-name func-name))) new-func-names) (make-list (length ref-vars) :initial-element :#f) (set-difference closure-list func-names)))
               ;(y (print `(:heap-list ,free-variables)))
               (next-all-variables (car next-finder-env))
               (next-free-variables (filter-free-variables next-all-variables))
               (next-free-funcs (intersection next-free-variables func-names))
e              (top-sym (car new-func-names))
               (k-sym (if (find top-sym next-free-funcs) top-sym (cps-gensym parser)))
               (next-free-funcs-without-k-sym (remove k-sym next-free-funcs))
               ;(x (print `(:nfree ,next-free-funcs :fn ,func-names)))
               (wrapped-record-offs-cps (if next-free-funcs (make-wrapped-record-offs parser k-sym func-names next-free-funcs-without-k-sym new-next-cps) new-next-cps))
               (wrapped-heap-cps (if ref-vars (make-wrapped-heap parser k-sym ref-vars func-names next-free-funcs wrapped-record-offs-cps) wrapped-record-offs-cps))

               (heap-cps
                 `(:HEAP ,heap-list (,k-sym) (,wrapped-heap-cps))))

          `(:FIXH ,new-binds ,heap-cps))))))

;----------------------------------------------------------------
; primitive & app
;----------------------------------------------------------------
;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser closure-converter) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (let* ((new-id `((:primitive . ,op) ,@result))
           (new-env (make-new-env parser env new-id))
           (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps new-env)) next-cpss)))

      `(,op ,args ,result ,new-next-cpss))))


;----------------------------------------------------------------
(def-cps-func cps-app ((parser closure-converter) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))

    (let ((closure-name func-name)
          (sym0 (cps-gensym parser)))
      (copy-tree `(:RECORD-REF ,(list closure-name 0) (,sym0) ((:APP ,sym0 (,closure-name ,@args))))))))

