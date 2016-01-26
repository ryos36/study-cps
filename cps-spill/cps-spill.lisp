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
  (labels ((build-spill-list (old-spill-list add-spill-vars)
            (if (null add-spill-vars)
              old-spill-list
              (let* ((spill-sims (car old-spill-list))
                     (new-spill-sims (cons (cps-gensym parser) spill-sims)))
                (cons new-spill-sims
                      (append (cdr old-spill-list) add-spill-vars))))))

    (let* ((max-n (max-n parser))
           (live-vars-list (cdr op-live-vars-list))
           (declare-vars (cdadr live-vars-list))
           (declare-vars-n (length declare-vars))
           (last-use-vars (cdaddr live-vars-list))
           (last-use-vars-n (length last-use-vars))
           (live-vars (cdr (cadddr live-vars-list)))
           (x (print `(:declare-vars ,declare-vars)))

           (spill-vars-list (cdr spill-list))
           (used-vars (cdr (car spill-vars-list)))
           (dup-room-n-vars (cdr (cadr spill-vars-list)))
           (dup-room-n (car dup-room-n-vars))
           (dup-vars (cdr dup-room-n-vars))
           (spill-out-list (cdr (caddr spill-vars-list)))
           (spill-out-vars (cdr spill-vars-list))

           (current-used-vars (union used-vars (union last-use-vars live-vars)))
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
           (max-dup-n (if need-spill? max-n
                        (if (null spill-out-vars) 0
                          dup-room-n)))
           (next-dup-n (max (min current-dup-n max-dup-n) 0))
           (next-dup-vars (if (or (= next-dup-n 0) (= max-n current-max-n)) nil
                            (if need-spill?
                              (union dup-vars used-vars)
                              dup-vars)))
           (next-spill-out-list (build-spill-list spill-out-list new-spill-out-vars)))

      (print `(:up ,next-dup-n ,@next-dup-vars))

      (values 
        (copy-tree
          `(:spill
             (:used ,@next-used-vars)
             (:duplicate ,next-dup-n ,@next-dup-vars)
             (:spill-out ,@next-spill-out-list)))
        need-spill?))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-spill) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))

        (live-vars-pair (caar env))
        (spill-list (cadar env)))

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

      (print `(:fix-cps-binds ,(mapcar #'(lambda (f) (car f)) binds) 
                              ,(mapcar #'(lambda (x) (car x)) binds-live-list)))
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

      (print `(:next-of-cps-bind ,(car next-cps)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(defmethod update-variables ((parser cps-spill) vars next-spill-list)
  (let ((vars-list (cdr next-spill-list))
        ref-vars)
    (print `(uv ,vars ,next-spill-list))
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
                              (setf (cdar next-spill-list) new-used-vars)
                              (setf (cdadr next-spill-list) 
                                    (cons new-dup-n new-dup-vars))
                              var)
                            (let* ((spill-vars (cdr spill-list))
                                   (pos (position var (reverse spill-vars))))
                              (if pos 
                                (let ((new-sym (cps-gensym parser)))
                                  (push (copy-list 
                                          `(:ref (,(caar spill-list) ,pos)
                                                 (,new-sym))) ref-vars)
                                  new-sym)
                                var ))))))))
              vars)
      ref-vars)))


;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-parser) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))

        (live-vars-pair (caar env))
        (spill-list (copy-tree (cadar env))))
        
    (let ((new-func-name (cps-symbol parser func-name env))
          (new-args (update-variables parser args spill-list)))
      ;    (new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args)))
      (print `(:app-new-args ,new-args))
      `(:APP ,new-func-name ,new-args))))
;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-spill) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))

        (live-vars-pair (caar env))
        (spill-list (cadar env)))

    (print `(:op ,op ,spill-list))
    (multiple-value-bind (next-spill-list need-spill?)  (update-next-spill-list parser live-vars-pair spill-list)

        (if need-spill?
          (print `(:update-result ,next-spill-list ,need-spill?)))
        (let* ((new-args (update-variables parser args next-spill-list))
               (next-live-vars-list (nth 5 live-vars-pair))
               (x (print `(:new-args ,new-args)))
               (new-next-cpss (mapcar #'(lambda (cps next-live-vars) 
                                          (let ((new-env (make-new-env parser env 
                                                                       `((:live-vars ,@next-live-vars)
                                                                         ,next-spill-list))))
                                            (cps-parse parser cps new-env)))
                                      next-cpss next-live-vars-list)))

          `(,op ,new-args ,result ,new-next-cpss)))))
