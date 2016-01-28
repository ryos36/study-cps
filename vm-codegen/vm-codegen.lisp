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
(defmethod make-jump-instruction ((codegen vm-codegen) sym)
  (copy-tree `(:jump (:attribute) ,sym))) 

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
        (codegen-tagged-list (cadar env))
        (heap-parser (heap-parser codegen)))

    (let ((app-info-list (find-app-for-branch-prediction codegen (cdr live-vars-tagged-list))))
      (add-app-info codegen codegen-tagged-list app-info-list)
      (reset-size heap-parser)
      (cps-fix heap-parser expr env)
    (print `(:cps-fix ,codegen-tagged-list ,(heap-size heap-parser) ,(cps-gensym codegen) ,(cps-gensym codegen t)))
    )    
    (print `(:lvtl ,(cddr live-vars-tagged-list)))

    (let ((next-env (pop-one-make-new-env codegen env
                                          #'(lambda (e)
                                              (car 
                                                   (nth 4
                                                     (cadddr
                                                       (caar e)))))))
          (bind-env (make-new-env codegen
                                   env
                                   `((:live-vars ,(caddr live-vars-tagged-list))
                                     ,codegen-tagged-list))))

      (if (use-jump-for-fix codegen)
        (let* ((label-sym (cps-gensym codegen t))
               (jump-insn (add-code codegen (make-jump-instruction codegen label-sym)))
               (new-binds (cps-binds codegen binds bind-env))
               (x (print `(:n-cps ,next-cps)))
               (label-insn (add-code codegen label-sym))
               (new-next-cps (cps-parse codegen next-cps next-env)))
          `(,fix-op ,new-binds ,new-next-cps))
        (let ((new-next-cps (cps-parse codegen next-cps next-env)))
          (new-binds (cps-binds codegen binds bind-env))
          `(,fix-op ,new-binds ,new-next-cps))))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((codegen vm-codegen) binds env)
  (let* ((live-vars-tagged-list (caar env))
         (binds-live-list (cdr live-vars-tagged-list))
         (codegen-tagged-list (cadar env)))

     (mapcar #'(lambda (bind live-vars)
                 (let ((new-env (make-new-env codegen
                                              env
                                              `((:live-vars ,@live-vars)
                                                ,codegen-tagged-list))))
                   (cps-bind codegen bind new-env))) binds binds-live-list)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-parser) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env)))

    (let ((new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,args ,new-next-cps))))
