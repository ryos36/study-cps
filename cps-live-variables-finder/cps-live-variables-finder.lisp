;----------------------------------------------------------------
(in-package :cps-live-variables-finder)

;----------------------------------------------------------------
(defclass cps-live-variables-finder (cps-parser)
  (()))

;----------------------------------------------------------------
(def-cps-func update-live-variables ((parser cps-live-variables-finder) vars env)
  (labels ((update-live-variables0 (vars0 top-env0)
             (if (or (null vars0) (null top-env0)) t
               (let* ((op-vars (car top-env0))
                      (vars-list (cdr op-vars))
                      (declare-vars (car vars-list))
                      (next-vars0 (set-difference vars0 (cdr declare-vars)))
                      (use-vars (cadr vars-list))
                      (to-live-vars (intersection vars0 (cdr use-vars))))

                 ;(print `(:to-live-vars ,to-live-vars))
                 (if to-live-vars
                   (let ((live-vars (caddr vars-list)))
                     ;(print `(:live-vars ,live-vars))
                     (setf (cdr live-vars) (append to-live-vars (cdr live-vars)))
                     (setf (cdr use-vars) (set-difference (cdr use-vars) to-live-vars))))
                 (update-live-variables0 next-vars0 (cdr top-env0)))))

           (update-live-variables1 (env1)
            (if (null env1) t
              (let ((top-env (car env1)))
                (update-live-variables0 vars top-env)
                (update-live-variables1 (cdr env1))))))

    (update-live-variables1 env)))

;----------------------------------------------------------------
(def-cps-func add-vars ((parser cps-live-variables-finder) declare-vars use-vars env)
  (let ((top-env (car env))
        (use-vars-ignore-not-symbol 
          (remove-if #'(lambda (x) (not (cps-symbolp x))) use-vars)))

    (push (copy-tree `(:op (:declare ,@declare-vars) 
                           (:use ,@use-vars-ignore-not-symbol)
                           (:live ))) top-env)

    (setf (car env) top-env)))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-live-variables-finder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr)))

    (let ((func-names (mapcar #'(lambda (bind) (car bind)) binds)))
        
      (add-vars parser (copy-list func-names) nil env)

      (let ((new-binds (cps-binds parser binds env))
            (new-next-cps (cps-parse parser next-cps env)))

        (copy-tree `(,fix-op ,new-binds
                             (:FIX-BODY
                               (:declare ,@func-names)
                               (:use)
                               (:live)
                               (,new-next-cps))))))))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-live-variables-finder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (another-env (make-new-env parser '() '())))

    (let ((declare-vars `(,func-name ,@args)))
      (add-vars parser (copy-list declare-vars) nil another-env)
      (let* ((new-next-cps (cps-parse parser next-cps another-env)))

        (copy-list `(:BIND (:declare ,@declare-vars) (:use) (:live)
                           (,new-next-cps)))))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-live-variables-finder) expr env)
  (let* ((func-name (cadr expr))
         (args (caddr expr))
         (use-vars-ignore-not-symbol 
           (remove-if #'(lambda (x) (not (cps-symbolp x))) args))
         (use-vars `(,func-name ,@use-vars-ignore-not-symbol)))
        
      ;(print `(:use-vars ,args :uv ,use-vars))
      ;(update-live-variables nil use-vars env)

      (copy-tree `(:APP (:declare) (:use ,@use-vars) (:live )))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-live-variables-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (top-env (car env))
        branches)

    (update-live-variables parser args env)
    (add-vars parser result args env)
    ;(print `(:cps-primitive ,top-env))

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) 
                                     (let* ((new-env (make-new-env parser env '()))
                                            (rv (cps-parse parser cps new-env)))
                                       (push (cdr new-env) branches)
                                       rv))
                                 next-cpss))
          (re-top-env (caar env)))

      ;(print `(:branches ,(length branches) ,branches))
      ;(setf (car env) (cons (cons :branches (nreverse branches)) top-env))

      `(,op ,(cadr re-top-env)
            ,(caddr re-top-env)
            ,(cadddr re-top-env)
                   ,new-next-cpss))))
