;----------------------------------------------------------------
(in-package :vm-codegen)

;----------------------------------------------------------------
(defclass vm-codegen (cps-parser)
  ((max-n :initarg :max-n :initform 5 :reader max-n)
   (codes :accessor codes :initform nil)
   (heap-parser :initform (make-instance 'heap-parser) :reader heap-parser)
   (live-variables-finder :initarg :live-variables-finder :reader live-variables-finder)
   (use-jump-for-fix :initform t :initarg :use-jump-for-fix :reader use-jump-for-fix)))

;----------------------------------------------------------------
(defmethod pop-one-make-new-env ((codegen vm-codegen) env &optional getter)
  (let* ((live-vars-tagged-list (caar env))
         (codegen-tagged-list (cadar env))
         (finder (live-variables-finder codegen))
         (top-cont-list 
           (if getter (funcall getter env)
             (car (cps-live-variables-finder:get-cont-list finder live-vars-tagged-list))))
         (the-new-env (copy-tree `((:live-vars ,top-cont-list)
                                   ,codegen-tagged-list))))

    (make-new-env codegen env the-new-env)))
;----------------------------------------------------------------
(defmethod add-code ((codegen vm-codegen) code)
  (push code (codes codegen))
  code)

;----------------------------------------------------------------
(defmethod get-final-codes ((codegen vm-codegen))
  (reverse
    (slot-value codegen 'codes)))

;----------------------------------------------------------------
(defmethod print-codes ((codegen vm-codegen))
  (dolist (insn (get-final-codes codegen))
    (format t "~s~%" insn)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod make-jump-instruction ((codegen vm-codegen) sym)
  (copy-tree `(:jump (:attribute) ,sym))) 

;----------------------------------------------------------------
(defmethod make-live-reg-instruction ((codegen vm-codegen) heap-size args live-args)
  (let* ((args-status (mapcar #'(lambda (a) (if (find a live-args) 1 0)) args))
         (reg-status (append args-status (make-list (- (max-n codegen) (length args)) :initial-element 0))))

    (copy-tree `(:live-reg (:attribute) ,heap-size ,reg-status)))) 

;----------------------------------------------------------------
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
(defmethod add-app-info ((codegen vm-codegen) codegen-tagged-list app-info-list)
  (let ((app-info-tagged-list (caddr codegen-tagged-list)))
    (setf (cdr app-info-tagged-list) app-info-list)))

;----------------------------------------------------------------
(def-cps-func cps-fix ((codegen vm-codegen) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env)))

#|
    (let* ((fix-list (cadr live-vars-tagged-list))
           (fix-id (car fix-list))
           (b-list (cadr fix-list))
           (b-len (length b-list))
           (b-dec-list (mapcar #'(lambda (b) 
                                   (list (car b) 
                                         (cadr b)
                                         (car (caar (cddddr b)))))
                               b-list))
           (fix-body (caddr fix-list))
           (fix-body-id (car fix-body))
           (next-id (caaar (cddddr fix-body))))

      (print `(:cps-fix ,fix-op 
                        :fix-id ,fix-id 
                        :bind ,b-len ,b-dec-list 
                        :fix-body-id ,fix-body-id 
                        :next ,next-id))
      (print `(:lvtl , (cadr live-vars-tagged-list)))
      (assert codegen-tagged-list))
|#

    (let* ((fix-list (cadr live-vars-tagged-list))
           (fix-body (caddr fix-list))
           (next-var-info (caar (cddddr fix-body)))
           (next-env (make-new-env codegen
                                  env
                                  `((:live-vars ,next-var-info)
                                    ,codegen-tagged-list)))
           (binds-env (make-new-env codegen
                                   env
                                   `((:live-vars ,(cadr fix-list))
                                     ,codegen-tagged-list))))

      (if (use-jump-for-fix codegen)
        (let* ((label-sym (cps-gensym codegen t))
               (jump-insn (add-code codegen (make-jump-instruction codegen label-sym)))
               (new-binds (cps-binds codegen binds binds-env))
               (label-insn (add-code codegen label-sym))
               (new-next-cps (cps-parse codegen next-cps next-env)))
          `(,fix-op ,new-binds ,new-next-cps))
        (let ((new-next-cps (cps-parse codegen next-cps next-env)))
          (new-binds (cps-binds codegen binds binds-env))
          `(,fix-op ,new-binds ,new-next-cps))))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((codegen vm-codegen) binds env)

  (let* ((live-vars-tagged-list (caar env))
         (binds-live-list (cadr live-vars-tagged-list))
         (codegen-tagged-list (cadar env)))

    (assert codegen-tagged-list)
    (mapcar #'(lambda (bind live-vars)
                (let ((new-env (make-new-env codegen
                                             env
                                             `((:live-vars ,live-vars)
                                               ,codegen-tagged-list))))
                  (cps-bind codegen bind new-env))) binds binds-live-list)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((codegen vm-codegen) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env))
        (heap-parser (heap-parser codegen))

        (finder (live-variables-finder codegen)))

    (assert codegen-tagged-list)
    (let* ((bind-vars-info (cadr live-vars-tagged-list))
           (app-info-list (find-app-for-branch-prediction codegen bind-vars-info))
           (live-tagged-list (cadddr bind-vars-info))
           (live-vars (cdr live-tagged-list)))

      (add-app-info codegen codegen-tagged-list app-info-list)
      (reset-size heap-parser)
      (cps-bind heap-parser expr env)
      (add-code codegen (make-live-reg-instruction codegen (heap-size heap-parser) args
        (reduce #'union 
            (mapcar #'(lambda (app-info) (cdr app-info)) app-info-list) :initial-value live-vars )))

      (let* ((next-env (make-new-env codegen
                                   env
                                   `((:live-vars ,(car (nth 4 bind-vars-info)))
                                     ,codegen-tagged-list)))
             (new-next-cps (cps-parse codegen next-cps next-env)))

      `(,func-name ,args ,new-next-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-app ((codegen vm-codegen) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env)))
        
    (add-code codegen (make-jump-instruction codegen (car args)))
    (let ((new-func-name (cps-symbol codegen func-name env))
          (new-args (mapcar #'(lambda (arg) (cps-terminal codegen arg env)) args)))
      `(:APP ,new-func-name ,new-args))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((codegen vm-codegen) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env)))

    (add-code codegen (copy-list `(,op (:attribute) ,args ,result))) ; dummy
    (assert codegen-tagged-list)

    (let* ((op-vars-info (cadr live-vars-tagged-list))
           (next-live-vars-list (car (cddddr op-vars-info)))
           (new-next-cpss (mapcar #'(lambda (cps next-live-vars)
                                      (let ((next-env (make-new-env codegen env
                                                                    `((:live-vars ,next-live-vars)
                                                                      ,codegen-tagged-list))))
                                        (cps-parse codegen cps next-env)))
                                  next-cpss
                                  next-live-vars-list
                                  )))

      `(,op ,args ,result ,new-next-cpss))))
