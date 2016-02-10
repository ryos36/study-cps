;----------------------------------------------------------------
(in-package :vm-codegen)

;----------------------------------------------------------------
(defclass vm-codegen (cps-parser)
  ((max-n :initarg :max-n :initform 10 :reader max-n)
   (registers :accessor registers :initform '
              (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))
   (codes :accessor codes :initform nil)
   (heap-parser :initform (make-instance 'heap-parser) :reader heap-parser)
   (live-variables-finder :initarg :live-variables-finder :reader live-variables-finder)
   (use-jump-for-fix :initform t :initarg :use-jump-for-fix :reader use-jump-for-fix)
   (use-attribute :initarg :use-attribute :initform nil :reader use-attribute)
   (initialize-codes :accessor initialize-codes)
   (global-variable :initform '(common-lisp-user::main common-lisp-user::exit) :reader global-variable)))

;----------------------------------------------------------------
(defmethod reset-registers ((codegen vm-codegen) registers)
  (setf (max-n codegen) (length registers))
  (setf (registers codegen) registers))

;----------------------------------------------------------------
(defmethod reset-codes ((codegen vm-codegen))
  (setf (codes codegen) nil)
  (add-code codegen 
            (make-label 'common-lisp-user::main :closure-name)))

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
  (append
    (initialize-codes codegen)
    (reverse
      (slot-value codegen 'codes))))

;----------------------------------------------------------------
(defmethod print-codes ((codegen vm-codegen) &optional (str t))
  (dolist (insn (get-final-codes codegen))
    (format str "~s~%" insn)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod create-initialize-codes ((codegen vm-codegen))
  (setf (initialize-codes codegen)
        (let ((main 'common-lisp-user::main)
              (exit 'common-lisp-user::exit))

        (list
          (make-jump-instruction codegen (closure-name-to-label-name main))
          (make-label main)
          (make-const-instruction codegen (closure-name-to-label-name main))
          (make-label exit)
          (make-const-instruction codegen (closure-name-to-label-name exit))
          (make-label exit :closure-name)
          (make-halt-instruction codegen)))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod make-attribute ((codegen vm-codegen)) (if (use-attribute codegen) (copy-list '((:attribute))) '()))

;----------------------------------------------------------------
(defmethod make-primitive-instruction ((codegen vm-codegen) op args result)
  `(,op ,@(make-attribute codegen) ,args , result))

;----------------------------------------------------------------
(defmethod make-jump-instruction ((codegen vm-codegen) label-sym)
  `(:jump ,@(make-attribute codegen) ,label-sym))

;----------------------------------------------------------------
(defmethod make-jump-cond-instruction ((codegen vm-codegen) op new-args label-sym)
  `(:jump-cond ,@(make-attribute codegen) ,op ,new-args ,label-sym))

;----------------------------------------------------------------
(defmethod make-live-reg-instruction ((codegen vm-codegen) heap-size args live-args)
  (let* ((args-status (mapcar #'(lambda (a) (if (find a live-args) 1 0)) args))
         (reg-status (append args-status (make-list (- (max-n codegen) (length args)) :initial-element 0))))

    `(:live-reg ,@(make-attribute codegen) ,heap-size ,reg-status)))

;----------------------------------------------------------------
(defmethod make-move-instruction ((codegen vm-codegen) reg-no0 reg-no1)
  (let ((registers (registers codegen)))
    ;(print `(:reg-no ,reg-no0 ,reg-no1))

  `(:move ,@(make-attribute codegen) ,(elt registers reg-no0) ,(elt registers reg-no1))))

;----------------------------------------------------------------
(defmethod make-swap-instruction ((codegen vm-codegen) reg-no0 reg-no1)
  (let ((registers (registers codegen)))
    ;(print `(:swap-reg-no ,reg-no0 ,reg-no1))

  `(:swap ,@(make-attribute codegen) ,(elt registers reg-no0) ,(elt registers reg-no1))))

;----------------------------------------------------------------
(defmethod make-movei-instruction ((codegen vm-codegen) imm reg-no)
  (let ((registers (registers codegen)))
  `(:movei ,@(make-attribute codegen) ,imm ,(elt registers reg-no))))

;----------------------------------------------------------------
(defmethod make-halt-instruction ((codegen vm-codegen) &optional (arg :r0))
  `(:halt ,@(make-attribute codegen) ,arg))

;----------------------------------------------------------------
(defmethod make-const-instruction ((codegen vm-codegen) const-value)
  `(:const ,@(make-attribute codegen) ,const-value))

;----------------------------------------------------------------
(defmethod add-global-variable ((codegen vm-codegen) sym)
  (setf (slot-value codegen 'global-variable)
        (cons sym (global-variable codegen))))

;----------------------------------------------------------------
(defmethod global-variable? ((codegen vm-codegen) sym)
  (find sym (global-variable codegen)))

;----------------------------------------------------------------

(defmethod set-global-variable-address-to-reg ((codegen vm-codegen) sym reg-no)
  ;(assert nil)
  (add-code codegen
            (let ((registers (registers codegen)))
              `(:movei ,@(make-attribute codegen) ,(make-label sym :closure-name)
                       ,(elt registers reg-no)))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod update-register-usage-for-bind ((codegen vm-codegen) codegen-tagged-list args live-vars)
  (let ((new-registers (mapcar #'(lambda (arg) (if (find arg live-vars) arg nil)) args))
        (register-tagged-list (cadr codegen-tagged-list)))

    (assert (eq (car register-tagged-list) :register))
    (mapl #'(lambda (reg-status-list new-reg-list)
              (setf (car reg-status-list) (car new-reg-list)))
          (cadr register-tagged-list)
          new-registers)))

;----------------------------------------------------------------
(defmethod get-empty-register-no ((codegen vm-codegen) codegen-tagged-list args)
    (let* ((register-tagged-list (cadr codegen-tagged-list))
           (register-list (cadr register-tagged-list))
           (app-info-tagged-list (caddr codegen-tagged-list)))

      (labels ((find-reg-in-app (expr app-list)
                (if (null app-list) nil
                  (let* ((app-tagged-list (car app-list))
                         (app-vars (cdr app-tagged-list))
                         (pos (position expr app-vars)))
                    (if (null (elt register-list pos)) pos
                      (find-reg-in-app expr (cdr app-list))))))

               (find-empty-reg-no ()
                  (let* ((len (length register-list))
                         (n-pos (position nil (reverse register-list)))
                         (pos (- len n-pos 1)))
                    pos)))

        (let* ((arg-list0 (mapcar #'(lambda (arg) (if (cps-symbolp arg) arg :NOT-SYMBOL)) args))
               (arg-list1 (mapcar #'(lambda (arg) (let ((found (find arg register-list)))
                                                    (if found found arg))) arg-list0))
               (pos-list0 (mapcar #'(lambda (arg) (find-reg-in-app arg (cdr app-info-tagged-list))) arg-list1))
               (pos-list1 (mapcar #'(lambda (pos) (if pos pos (find-empty-reg-no))) pos-list0)))
          (mapc #'(lambda (pos) (assert pos)) pos-list1)
          pos-list1))))

;----------------------------------------------------------------
(defmethod update-register-usage ((codegen vm-codegen) codegen-tagged-list args env)
  (let* ((register-tagged-list (cadr codegen-tagged-list))
         (register-list (cadr register-tagged-list))
         (app-info-tagged-list (caddr codegen-tagged-list)))

    (labels ((find-reg-in-app (expr app-list)
                (if (null app-list) nil
                  (let* ((app-tagged-list (car app-list))
                         (app-vars (cdr app-tagged-list))
                         (pos (position expr app-vars)))
                    (if (and pos (null (elt register-list pos))) pos
                      (find-reg-in-app expr (cdr app-list))))))

               (find-empty-reg-no ()
                  (let* ((len (length register-list))
                         (n-pos (position nil (reverse register-list)))
                         (pos (- len n-pos 1)))
                    pos))

               (need-not-find (arg)
                 (or (eq :NOT-SYMBOL arg) (numberp arg))))

      (let* ((registers (registers codegen))
             ;(x (print `(:args ,args)))
             (arg-list0 (mapcar #'(lambda (arg) (if (cps-symbolp arg) arg :NOT-SYMBOL)) args))
             ;(x (print `(:arg-list0 ,arg-list0 ,register-list)))
             (arg-list1 (mapcar #'(lambda (arg) (let ((found (position arg register-list)))
                                                  (if found found arg))) arg-list0))
             ;(x (print `(:arg-list1 ,arg-list1)))
             (pos-list0 (mapcar #'(lambda (arg) (if (need-not-find arg) arg (find-reg-in-app arg (cdr app-info-tagged-list)))) arg-list1))
             ;(x (print `(:pos-list0 ,pos-list0)))
             (pos-list1 (mapcar #'(lambda (pos0) 
                                    (if pos0 pos0 (find-empty-reg-no))) pos-list0)))

        ;(print `(:pos-list1 ,pos-list1))

        (mapcar #'(lambda (pos arg)
                    (assert (need-not-find pos))
                    ;(print `(:pos ,pos ,(numberp pos) ,(if (numberp pos) (elt register-list pos))))
                    (if (numberp pos)
                      (if (null (elt register-list pos))
                        (setf (elt register-list pos) arg)))

                    ;(print `(:pos-list1 ,pos ,arg , (if (numberp pos) (elt register-list pos) :?) ,register-list))
                    (cps-terminal codegen arg env))
                pos-list1
                args)))))

;----------------------------------------------------------------
(defmethod update-register-not-used ((codegen vm-codegen) codegen-tagged-list live-vars-tagged-list)
  ;(print `(:update ,codegen-tagged-list ,live-vars-tagged-list))
  (let* ((register-tagged-list (cadr codegen-tagged-list))
         (register-list (cadr register-tagged-list))

         (live-vars-list (cadr live-vars-tagged-list))
         (declare-tagged-list (cadr live-vars-list))
         (declare-vars (cdr declare-tagged-list))

         (use-tagged-list (caddr live-vars-list))
         (use-vars (cdr use-tagged-list))

         (live-tagged-list (cadddr live-vars-list))
         (live-vars (cdr live-tagged-list))

         (not-used-reg-for-bind (set-difference declare-vars live-vars))
         (not-used-reg use-vars))

    ;(print `(:before-register-list ,register-list ,not-used-reg))
    (mapc #'(lambda (arg) (let ((pos (position arg register-list)))
                            ;(print `(:pos ,pos :not-used-reg ,not-used-reg))
                            (assert pos)
                            (setf (elt register-list pos) nil))) not-used-reg)

    ;(print `(:remove-register-list ,register-list))
    not-used-reg))

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

      #|
      (print `(:cps-fix ,fix-op 
                        :fix-id ,fix-id 
                        :bind ,b-len ,b-dec-list 
                        :fix-body-id ,fix-body-id 
                        :next ,next-id))
      (print `(:lvtl , (cadr live-vars-tagged-list)))
      |#
      (assert codegen-tagged-list))
|#

    (let* ((fix-list (cadr live-vars-tagged-list))
           (fix-body (caddr fix-list))
           (next-var-info (caar (cddddr fix-body)))
           (next-env (make-new-env codegen
                                  env
                                  `((:live-vars ,next-var-info)
                                    ,(copy-tree codegen-tagged-list))))
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
                                               ,(copy-tree codegen-tagged-list)))))
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
      (add-code codegen func-name)

      (update-register-usage-for-bind codegen codegen-tagged-list args live-vars)
      (let* ((next-env (make-new-env codegen
                                   env
                                   `((:live-vars ,(car (nth 4 bind-vars-info)))
                                     ,codegen-tagged-list)))
             (new-args (mapcar #'(lambda (arg) (cps-terminal codegen arg next-env)) args))
             ;(x (print `(:args ,args :=> ,new-args)))
             (new-next-cps (cps-parse codegen next-cps next-env)))

      `(,func-name ,new-args ,new-next-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-app ((codegen vm-codegen) expr env)
  ;(print `(:cps-app ,expr ,env))
  (let ((func-name (cadr expr))
        (args (caddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env)))
        
    (let* ((register-tagged-list (cadr codegen-tagged-list))
           (register-list (cadr register-tagged-list))
           (registers (registers codegen)))

      ;(print `(:register-list ,register-list :args ,args))

      (let ((cur-pos 0))
        (mapl #'(lambda (arg-list reg-list) 
                  (let ((arg (car arg-list))
                        (sym (car reg-list)))

                    ;(print `(:0register-list ,register-list :args ,args))
                    (if (global-variable? codegen arg)
                      (set-global-variable-address-to-reg codegen arg cur-pos)

                      (if (eq arg sym) :already-set 
                        (if (cps-symbolp arg)
                          (let ((pos (position arg register-list)))
                            (assert pos)
                            (let ((new-pos (position sym (cdr arg-list))))
                              ;(print `(:new-pos ,arg ,new-pos))
                              (if new-pos
                                (let* ((abs-new-pos (+ cur-pos new-pos))
                                       (disappeared-sym (elt register-list abs-new-pos)))
                                  ;(print `(:elt ,cur-pos ,new-pos ,abs-new-pos ,disappeared-sym))
                                  (if (find disappeared-sym (cdr arg-list))
                                    (progn
                                      ;(print `(:elt ,register-list ,(elt register-list pos)))

                                      (add-code codegen (make-swap-instruction codegen pos cur-pos))
                                      (setf (elt register-list cur-pos) arg)
                                      (setf (elt register-list pos) disappeared-sym))
                                    (progn 
                                      (add-code codegen (make-move-instruction codegen cur-pos abs-new-pos))
                                      (add-code codegen (make-move-instruction codegen pos cur-pos)))))
                                (add-code codegen (make-move-instruction codegen pos cur-pos)))))
                          
                          (add-code codegen (make-movei-instruction codegen arg cur-pos)))))

                  (incf cur-pos)))

                          args (copy-list register-list)))

      ;(print `(:func-name ,func-name ,register-list))
      (let* ((pos (position func-name register-list))
             (reg (elt registers pos)))
        (add-code codegen (make-jump-instruction codegen reg))

        `(:APP ,reg ,(butlast registers (- (length registers) (length args))))))))

;----------------------------------------------------------------
(defmethod cps-compare-primitive ((codegen vm-codegen) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))

        (live-vars-tagged-list (caar env))
        (codegen-tagged-list (cadar env))

        (label-sym (cps-gensym codegen t)))

        (assert codegen-tagged-list)
        (assert (= 2 (length next-cpss)))
        ;(print `(:op ,op ,args ,result :codegen ,codegen-tagged-list))

        (let* ((live-vars-list (cadr live-vars-tagged-list))
               (declare-tagged-list (cadr live-vars-list))
               (declare-vars (cdr declare-tagged-list))
               (live-tagged-list (cadddr live-vars-list))
               (live-vars (cdr live-tagged-list))

               (not-used-reg (set-difference declare-vars live-vars))
               (new-args (update-register-usage codegen codegen-tagged-list args env))
               ;(x (print `(:op ,op)))
               (not-used-reg (update-register-not-used codegen codegen-tagged-list live-vars-tagged-list))
               (new-result (update-register-usage codegen codegen-tagged-list result env)))
          ; primitive-code
          (add-code codegen (make-jump-cond-instruction codegen op new-args label-sym))

          (let* ((else-clause (cadr next-cpss))
                 (then-clause (car next-cpss))
                 (op-vars-info (cadr live-vars-tagged-list))
                 (next-live-vars-list (car (cddddr op-vars-info)))

                 (next-env-for-else (make-new-env codegen env
                                       `((:live-vars ,(cadr next-live-vars-list))
                                         ,(copy-tree codegen-tagged-list))))
                 ;(x (print `(:else ,else-clause)))
                 (new-next-else-cps (cps-parse codegen else-clause next-env-for-else))
                 (label-insn (add-code codegen label-sym))

                 (next-env-for-then (make-new-env codegen env
                                       `((:live-vars ,(car next-live-vars-list))
                                         ,(copy-tree codegen-tagged-list))))

                 (new-next-then-cps (cps-parse codegen then-clause next-env-for-then)))

            `(,op ,new-args ,new-result (,new-next-then-cps ,new-next-else-cps))))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((codegen vm-codegen) expr env)
  (let ((op (car expr)))
    (if (compare-primitivep op)
      (cps-compare-primitive codegen expr env)
      (let ((args (cadr expr))
            (result (caddr expr))
            (next-cpss (cadddr expr))

            (live-vars-tagged-list (caar env))
            (codegen-tagged-list (cadar env)))

        (assert codegen-tagged-list)
        (assert (= 1 (length next-cpss)))
        ;(print `(:op ,op ,args ,result :codegen ,codegen-tagged-list))

        (let* ((live-vars-list (cadr live-vars-tagged-list))
               (declare-tagged-list (cadr live-vars-list))
               (declare-vars (cdr declare-tagged-list))
               (live-tagged-list (cadddr live-vars-list))
               (live-vars (cdr live-tagged-list))

               (not-used-reg (set-difference declare-vars live-vars))
               ;(x (print `(:live-vars-list ,live-vars-list)))
               (new-args (update-register-usage codegen codegen-tagged-list args env))
               (not-used-reg (update-register-not-used codegen codegen-tagged-list live-vars-tagged-list))
               (new-result (update-register-usage codegen codegen-tagged-list result env)))

          ;(print `(:op ,op (,(copy-tree args) :=> ,(copy-tree new-args) (,(copy-tree result) :=> ,(copy-tree new-result)))))
          ;(print `(:live-vars ,(copy-tree (cadr live-vars-tagged-list)) :not-used ,not-used-reg))
          ;(print `(:register-list ,(cadr (cadr codegen-tagged-list))))

          ; primitive-code
          (add-code codegen (make-primitive-instruction codegen op new-args new-result))

          (let* ((op-vars-info (cadr live-vars-tagged-list))
                 (next-live-vars-list (car (cddddr op-vars-info)))
                 (next-env (make-new-env codegen env
                                         `((:live-vars ,(car next-live-vars-list))
                                           ,codegen-tagged-list)))
                 (next-cps (car next-cpss))
                 (new-next-cpss (cps-parse codegen next-cps next-env)))

            `(,op ,new-args ,new-result (,new-next-cpss))))))))

;----------------------------------------------------------------
(def-cps-func cps-neq ((codegen vm-codegen) expr env)
    (cps-compare-primitive codegen expr env))

;----------------------------------------------------------------
(def-cps-func cps-exit ((codegen vm-codegen) expr env)
  (let* ((op (car expr))
         (arg (caadr expr))

         (live-vars-tagged-list (caar env))
         (codegen-tagged-list (cadar env))

         (register-tagged-list (cadr codegen-tagged-list))
         (register-list (cadr register-tagged-list)))

    (print `(:expr ,expr :env ,(car env)))

    (if (cps-symbolp arg)
      (let ((reg-pos (position arg register-list)))
        (if (not (= 0 reg-pos))
          (add-code codegen (make-move-instruction codegen reg-pos 0))))

      (add-code codegen (make-movei-instruction codegen arg 0)))

    (add-code codegen (make-halt-instruction codegen))))
;----------------------------------------------------------------
(def-cps-func cps-symbol ((codegen vm-codegen) expr env)
    (let* ((registers (registers codegen))
           (live-vars-tagged-list (caar env))
           (codegen-tagged-list (cadar env))
           (register-tagged-list (cadr codegen-tagged-list))
           (register-list (cadr register-tagged-list))
           (app-info-tagged-list (caddr codegen-tagged-list))
           (pos (position expr register-list)))

      (if pos (elt registers pos) nil)))

