;----------------------------------------------------------------
(in-package :vm-codegen)

;----------------------------------------------------------------
(defclass vm-codegen (cps-parser)
  ((max-n :initarg :max-n :initform 5 :reader max-n)
   (codes :writer codes :reader get-codes)))

;----------------------------------------------------------------
(defmethod add-code ((codegen vm-codegen) code)
  (push (codes codegen) code))

;----------------------------------------------------------------
(defmethod get-code ((codegen vm-codegen))
  (reverse
    (slot-value codegen 'codes)))

;----------------------------------------------------------------
(defmethod find-app-for-branch-prediction ((codegen vm-codegen) free-vars-expr)
  (let (rv)
    (labels ((free-vars-parse (expr0)
                (if (null expr0) t
                  (let ((op (car expr0)))
                    (case op
                      (:fixh (free-vars-fix expr0))
                      (:fixs (free-vars-fix expr0))
                      (:bind (free-vars-bind expr0))
                      (:app (free-vars-app expr0))
                      (otherwise
                        (let ((op-list (car (cddddr expr0))))
                          (free-vars-parse0 op-list)))))))

           (free-vars-parse0 (expr-list0)
              (if (null expr-list0) t
                (let ((one-op (car expr-list0)))
                  (free-vars-parse one-op)
                  (free-vars-parse0 (cdr expr-list0)))))

           (free-vars-fix (expr0)
              (let ((fix-body (caddr expr0))) 
                (free-vars-parse fix-body)))

           (free-vars-bind (expr0)
              (let ((op-list (car (cddddr expr0))))
                (free-vars-parse (car op-list))))

           (free-vars-app (expr0)
              (let ((args (cdaddr expr0)))
                (push (copy-tree `(:app ,@args)) rv))))

      (free-vars-parse free-vars-expr)
      rv)))

;----------------------------------------------------------------
(defmethod add-app-info ((codegen vm-codegen) codegen-tag-list app-info-list)
  (let ((app-info-tag-list (caddr codegen-tag-list)))
    (setf (cdr app-info-tag-list) app-info-list)))

;----------------------------------------------------------------
(def-cps-func cps-fix ((codegen vm-codegen) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))

        (live-vars-tag-list (caar env))
        (codegen-tag-list (cadar env)))

    (let ((app-info-list (find-app-for-branch-prediction codegen (cdr live-vars-tag-list))))
      (add-app-info codegen codegen-tag-list app-info-list)
    (print `(:cps-fix ,codegen-tag-list))
    (assert nil)
    )    
    (let ((new-binds (cps-binds parser binds env))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,fix-op ,new-binds ,new-next-cps))))
