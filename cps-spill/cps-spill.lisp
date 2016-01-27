;----------------------------------------------------------------
(in-package :cps-spill)

;----------------------------------------------------------------
(defclass cps-spill (cps-parser)
  ((max-n :initarg :max-n :initform 5 :reader max-n)))

;----------------------------------------------------------------
(defun make-new-spill-list ()
  (copy-tree
    '(:spill
       (:used )
       (:duplicate 0) ; (:duplicate n vars...) 
       (:spill-out ))))

;----------------------------------------------------------------
(defmethod update-next-spill-list ((parser cps-spill) op-live-vars-list spill-list)
  ;(print `(:unsl ,spill-list))
  (labels ((build-spill-list (old-spill-list add-spill-vars)
            (if (null add-spill-vars)
              old-spill-list
              (let* ((spill-sims (car old-spill-list))
                     (new-spill-sims (cons (cps-gensym parser) spill-sims)))
                (cons new-spill-sims
                      (append (cdr old-spill-list) add-spill-vars))))))

    (let* ((max-n (max-n parser))
           (live-vars-list (cdr op-live-vars-list))
           (live-vars (cdr (cadddr live-vars-list)))

           (all-declare-vars (cdadr live-vars-list))
           (declare-vars (intersection all-declare-vars live-vars))
           (declare-vars-n (length declare-vars))
           (last-use-vars (cdaddr live-vars-list))
           (last-use-vars-n (length last-use-vars))

           (spill-vars-list (cdr spill-list))
           ;(x (print `(:spill ,spill-vars-list)))
           (used-vars (cdr (car spill-vars-list)))
           (dup-room-n-vars (cdr (cadr spill-vars-list)))
           (dup-room-n (car dup-room-n-vars))
           (dup-vars (cdr dup-room-n-vars))
           (spill-out-list (cdr (caddr spill-vars-list)))
           (spill-out-vars (cdr spill-vars-list))

           (current-used-vars (union used-vars (union last-use-vars (set-difference live-vars declare-vars))))
           (next-n (+ (length (set-difference current-used-vars last-use-vars))
                      (if (< last-use-vars-n declare-vars-n)
                        (- declare-vars-n last-use-vars-n)
                        0)
                      (if (null spill-out-list) 0 1)))

           (need-spill? (>= next-n max-n))

           (new-spill-out-vars (if need-spill? (set-difference used-vars last-use-vars) nil))

           (next-used-vars (union (set-difference current-used-vars 
                                                  (union new-spill-out-vars last-use-vars)) declare-vars))
           (current-max-n 
             (+ (max (length current-used-vars) (length next-used-vars))
                (if (null spill-out-vars) 0 1)))
           (current-dup-n 
             (if need-spill?
               (- max-n (length live-vars) (max last-use-vars-n declare-vars-n) 1)
               (- max-n current-max-n)))
           ;(x (print `(:min ,need-spill? ,current-dup-n ,current-max-n)))
           (max-dup-n (if need-spill? max-n
                        (if (null spill-out-vars) 0
                          dup-room-n)))
           ;(x (print `(:mmin ,need-spill? ,dup-room-n-vars)))
           (a (assert dup-room-n))
           (next-dup-n (max (min current-dup-n max-dup-n) 0))
           (next-dup-vars (if (or (= next-dup-n 0) (= max-n current-max-n)) nil
                            (if need-spill?
                              (union dup-vars used-vars)
                              dup-vars)))
           (next-spill-out-list (build-spill-list spill-out-list new-spill-out-vars)))

      ;(print `(:up ,next-dup-n ,@next-dup-vars))

      ;(print `(:next-dup-n ,next-dup-n))
      (values 
        (copy-tree
          `(:spill
             (:used ,@next-used-vars)
             (:duplicate ,next-dup-n ,@next-dup-vars)
             (:spill-out ,@next-spill-out-list)))
        need-spill?))))

;----------------------------------------------------------------
(defmethod update-variables ((parser cps-spill) vars next-spill-list)
  (let ((vars-list (cdr next-spill-list))
        pop-vars)
    (values
      (mapcar #'(lambda (var)
                  (if (not (cps-symbolp var)) var
                    (let ((used-vars (cdar vars-list))
                          (dup-n-vars (cdadr vars-list))
                          (spill-list (cdr (caddr vars-list))))

                      (if (find var used-vars) var
                        (let ((dup-vars (cdr dup-n-vars)))
                          (if (find var dup-vars)
                            (let* ((new-used-vars (cons var used-vars))
                                   (new-dup-n (- (car dup-n-vars) 1))
                                   (new-dup-vars (if (= new-dup-n 0) '()
                                                   (remove var dup-vars))))
                              (setf (cdadr next-spill-list) new-used-vars)
                              (setf (cdaddr next-spill-list) 
                                    (cons new-dup-n new-dup-vars))
                              var)
                            (let* ((spill-vars (cdr spill-list))
                                   (pos (position var (reverse spill-vars))))
                              (if pos 
                                (let ((new-sym (cps-gensym parser)))
                                  (push (copy-list 
                                          `(:pop-vars (,(caar spill-list) ,pos)
                                                 (,new-sym))) pop-vars)
                                  new-sym)
                                var ))))))))
              vars)
      pop-vars)))


;----------------------------------------------------------------
(defmethod create-reference-wrapper ((parser cps-spill) pop-vars)
  (let ((cont-list (copy-tree `(:CONT))))
    (labels ((create-reference-wrapper1 (pop-vars0 rv)
               (if (null pop-vars0) rv
                 (let ((one-pop-var (copy-tree (car pop-vars0)))
                       (insert-cont (list rv)))

                   (setf (cdddr one-pop-var) insert-cont)
                   (setf (car one-pop-var) :RECORD-REF)

                   (create-reference-wrapper1 (cdr pop-vars0) 
                                              (list one-pop-var))))))

      (let ((wrapped-cps (car (create-reference-wrapper1 pop-vars cont-list))))
        (flet ((reference-place-holder-func (cont)
                 (setf (car cont-list) cont)
                 wrapped-cps))
          #'reference-place-holder-func)))))

;----------------------------------------------------------------
(defmethod create-stack-wrapper ((parser cps-spill) old-spill-list new-spill-list)
  (let* ((old-spill-vars (cddr (cadddr old-spill-list)))
         (new-spill-out-list (cadddr new-spill-list))
         (new-spill-vars (cddr new-spill-out-list))
         (new-spill-syms (cadr new-spill-out-list))
         (the-spill-sym (car new-spill-syms))
         (add-spill-vars (last new-spill-vars (- (length new-spill-vars) 
                                                 (length old-spill-vars))))
         ;(x (print `(:asv ,add-spill-vars )))
         (stack-cps (copy-tree `(:STACK ,(reverse add-spill-vars) 
                                        (,the-spill-sym) (CONT))))
         (cont-list (pickup-list stack-cps 'CONT)))

      (flet ((stack-place-holder-func (cont)
                (setf (car cont-list) cont)
                stack-cps))

        #'stack-place-holder-func)))

;----------------------------------------------------------------
(defmethod create-pop-wrapper ((parser cps-spill) spill-list)
  (let* ((spill-vars (cddr (cadddr spill-list)))
         (pop-cps (copy-tree `(:POP (,(length spill-vars)) 
                                        () (CONT))))
         (cont-list (pickup-list pop-cps 'CONT)))

      (flet ((pop-place-holder-func (cont)
                (setf (car cont-list) cont)
                pop-cps))

        #'pop-place-holder-func)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-spill) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))

        (live-vars-pair (caar env))
        (spill-list (cadar env)))

    ;(print `(:cps-fix ,fix-op ,spill-list))
    (let* ((fix-body-live-vars-pair (cadddr live-vars-pair))
           (next-live-vars-list (car (nth 4 fix-body-live-vars-pair)))
           (binds-live-vars (caddr live-vars-pair))
           (binds-env (make-new-env parser env `((:live-vars ,@binds-live-vars)
                                                 (:no-spill))))
           (new-binds (cps-binds parser binds binds-env))
           (next-env (make-new-env parser env 
                                   `((:live-vars ,@next-live-vars-list)
                                     ,spill-list)))
           (new-next-cps (cps-parse parser next-cps next-env)))

      `(,fix-op ,new-binds ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((parser cps-spill) binds env)
  (let* ((live-vars-pair (caar env))
         (binds-live-list (cdr live-vars-pair))
         (spill-list (cdar env)))

      ;(print `(:fix-cps-binds ,(mapcar #'(lambda (f) (car f)) binds) ,(mapcar #'(lambda (x) (car x)) binds-live-list)))
      (mapcar #'(lambda (bind live-vars)
                  (let ((new-env (make-new-env parser
                                               env
                                               `((:live-vars ,@live-vars)
                                                 ,(make-new-spill-list)))))
                (cps-bind parser bind new-env))) binds binds-live-list)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-spill) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))

        (live-vars-pair (caar env))
        (spill-list (cadar env)))

    ;(print `(:cps-bind-spill-status ,func-name ,(car next-cps)))
    ;(print `(:next-live-varsssss ,(cdr live-vars-pair)))

    (let* ((next-live-vars (car (nth 5 live-vars-pair)))
           (next-spill-list (update-next-spill-list parser live-vars-pair spill-list))
           (bind-env (make-new-env parser env `((:live-vars ,@next-live-vars)
                                                ,next-spill-list)))
           (new-next-cps (cps-parse parser next-cps bind-env)))

      ;(print `(:next-of-cps-bind ,(car next-cps)))

      `(,func-name ,args ,new-next-cps))))


;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-spill) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))

        (live-vars-pair (caar env))
        (spill-list (copy-tree (cadar env))))
        
    (multiple-value-bind (new-args pop-vars)
      (update-variables parser args spill-list)
        (let* ((reference-wrapper-func (if pop-vars (create-reference-wrapper parser pop-vars) #'(lambda (x) x)))
               (spill-vars-list (cadddr spill-list))
               (pop-wrapper-func (if (cdr spill-vars-list) (create-pop-wrapper parser spill-list) #'(lambda (x) x))))

            (funcall reference-wrapper-func
              (funcall pop-wrapper-func
                 `(:APP ,func-name ,new-args)))))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-spill) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))

        (live-vars-pair (caar env))
        (spill-list (cadar env)))

    ;(print `(:op ,op ,args ,spill-list ,(copy-tree (car env))))
    (multiple-value-bind (next-spill-list need-spill?)  (update-next-spill-list parser live-vars-pair spill-list)

        #|
        (if need-spill?
          (print `(:update-result ,next-spill-list ,need-spill?)))
        |#

        (multiple-value-bind (new-args pop-vars)
          (update-variables parser args next-spill-list)

          (let* ((reference-wrapper-func (if pop-vars (create-reference-wrapper parser pop-vars) #'(lambda (x) x)))
                 (stack-wrapper-func (if need-spill? (create-stack-wrapper parser spill-list next-spill-list) #'(lambda (x) x)))
                 (next-live-vars-list (nth 5 live-vars-pair))
                 (new-next-cpss (mapcar #'(lambda (cps next-live-vars) 
                                            (let ((new-env (make-new-env parser env 
                                                                         `((:live-vars ,@next-live-vars)
                                                                           ,next-spill-list))))
                                              (cps-parse parser cps new-env)))
                                        next-cpss next-live-vars-list)))

            (funcall stack-wrapper-func
               (funcall reference-wrapper-func
                   `(,op ,new-args ,result ,new-next-cpss))))))))
