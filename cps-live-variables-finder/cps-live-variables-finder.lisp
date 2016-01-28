;----------------------------------------------------------------
(in-package :cps-live-variables-finder)

;----------------------------------------------------------------
(defclass cps-live-variables-finder (cps-parser)
  (()))

;----------------------------------------------------------------
(defparameter *tag-template*
  `(:op (:declare |...|) 
        (:use |...|)
        (:live |...|)

        (CONT |...|)))

(defparameter *tag-position*
  (mapcar #'(lambda (t-target)
              (if (atom t-target)
                nil
                (let ((tag-applicant (car t-target)))
                  (if (consp tag-applicant)
                    nil
                    (let ((tag tag-applicant))
                      tag))))) *tag-template* ))

;----------------------------------------------------------------
(defmacro create-get-vars-list-method (func-name class-object target-tag-list)
  (let ((target-pos-list (mapcar #'(lambda (tag) (position tag *tag-position*)) target-tag-list)))
    `(defmethod ,func-name (,class-object vars-expr)
       (mapcar #'(lambda (pos) (cdr (nth pos vars-expr)))
               ',target-pos-list ))))

;----------------------------------------------------------------
(create-get-vars-list-method get-vars (parser cps-live-variables-finder)
                             (:declare :use :live))

;----------------------------------------------------------------
(defmacro create-get-tagged-list-method (func-name class-object target-tag)
  (let ((target-pos (position target-tag *tag-position*)))
    `(defmethod ,func-name (,class-object vars-expr)
       (nth ,target-pos vars-expr))))

;----------------------------------------------------------------
(defmacro create-get-tagged-list-method-from-tag-list (tag-list)
  `(progn
     ,@(mapcar #'(lambda (tag)
        (let* ((func-name-string (format nil "GET-~a-TAGGED-LIST" tag))
               (func-name (intern func-name-string)))
          `(create-get-tagged-list-method ,func-name (parser cps-live-variables-finder) ,tag))) tag-list)))

;----------------------------------------------------------------
(create-get-tagged-list-method-from-tag-list (:declare :use :live))

;----------------------------------------------------------------
(defmethod update-live-variables ((parser cps-live-variables-finder) vars env)
  (labels ((update-live-variables0 (vars0 top-env0)
             ;(print `(:top-env0 ,(car top-env0) ,vars))
             (if (or (null vars0) (null top-env0)) t
               (let* ((op-vars (car top-env0))
                      (vars-list (cdr op-vars))
                      (declare-vars (car vars-list))
                      (next-vars0 (set-difference vars0 (cdr declare-vars)))
                      (use-vars (cadr vars-list))
                      (to-live-vars (intersection vars0 (cdr use-vars)))
                      (live-declare-vars (intersection vars0 (cdr declare-vars)))
                      (add-live-vars (union to-live-vars live-declare-vars)))

                 (assert (null (intersection to-live-vars live-declare-vars)))
                 ;(print `(:vars ,vars :declare ,declare-vars :=> ,live-declare-vars))
                 ;(print `(:to-live-vars ,to-live-vars))
                 ;(print `(:add-live-vars ,add-live-vars))
                 (if (or to-live-vars add-live-vars)
                   (let* ((live-tag-vars (caddr vars-list))
                          (live-vars (cdr live-tag-vars))
                          (add-new-live-vars (set-difference add-live-vars live-vars)))
                     ;(print `(:live-vars ,live-vars :+ ,add-new-live-vars))
                     (if add-new-live-vars
                       (setf (cdr live-tag-vars) (append add-new-live-vars live-vars)))
                     ;(print `(:live-tag-vars ,live-tag-vars))
                     ;(print `(:env ,env))
                 ;(assert nil)
                     (setf (cdr use-vars) (set-difference (cdr use-vars) to-live-vars))))
                 (update-live-variables0 next-vars0 (cdr top-env0)))))

           (update-live-variables1 (env1)
            (if (null env1) t
              (let ((top-env (car env1)))
                (update-live-variables0 vars top-env)
                (update-live-variables1 (cdr env1))))))

    (update-live-variables1 env)))

;----------------------------------------------------------------
(defmethod add-vars ((parser cps-live-variables-finder) declare-vars use-vars env)
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

    ; no meaning
    (add-vars parser (mapcar #'(lambda (bind) (car bind)) binds)  nil env)

    (let ((new-binds (cps-binds parser binds env))
          (new-next-cps (cps-parse parser next-cps env)))

      (copy-tree `(,fix-op ,new-binds
                           (:FIX-BODY
                             (:declare)
                             (:use)
                             (:live)
                             (,new-next-cps)))))))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-live-variables-finder) expr env)
  (let ((args (cadr expr))
        (next-cps (caddr expr))
        (another-env (make-new-env parser '() '())))

    (add-vars parser (copy-list args) nil another-env)
    (let* ((new-next-cps (cps-parse parser next-cps another-env))
           (top-item-of-top-env (caar another-env))
           (live-tag-vars (cadddr top-item-of-top-env))
           (live-vars (cdr live-tag-vars)))
      ;(print `(:live---------- ,live-tag-vars ,live-vars))

      (copy-list `(:BIND (:declare ,@args) (:use) (:live ,@live-vars)
                         (,new-next-cps))))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-live-variables-finder) expr env)
  (let* ((args (caddr expr))
         (use-vars-replace-not-symbol 
           (mapcar #'(lambda (x) (if (cps-symbolp x) x :NO-SYMBOL)) args)))
        
    ;(print `(:use-vars ,args :uv ,use-vars))
    ;(update-live-variables nil use-vars env)

    (copy-tree `(:APP (:declare) (:use ,@use-vars-replace-not-symbol) (:live )))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-live-variables-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (top-env (car env))
        branches)

    (let ((use-vars-ignore-not-symbol 
          (remove-if #'(lambda (x) (not (cps-symbolp x))) args)))

      (update-live-variables parser use-vars-ignore-not-symbol env)
    ;(print `(:cps-primitive ,op ,args ,env))
      (add-vars parser result use-vars-ignore-not-symbol env))


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
