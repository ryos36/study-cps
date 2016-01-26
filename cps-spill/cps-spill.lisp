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
(defmethod update-next-spill-list ((parser cps-spill) live-vars-list spill)
  (let* ((max-n (max-n parser))
         (declare-vars (cdadr live-vars-list))
         (declare-vars-n (length declare-vars))
         (last-use-vars (cdaddr live-vars-list))
         (last-use-vars-n (length last-use-vars))
         (live-vars (cdr (cadddr live-vars-list)))

         (used-vars (cdr (car spill)))
         (dup-room-n-vars (cdr (cadr spill)))
         (dup-room-n (car dup-room-n-vars))
         (dup-vars (cdr dup-room-n-vars))
         (spill-out-vars (cdr (caddr spill)))

         (current-used-vars (union used-vars (union last-use-vars live-vars)))
         (next-n (+ (length current-used-vars) 
                    (if (< last-use-vars-n declare-vars-n)
                      (- declare-vars-n last-use-vars-n)
                      0)
                    (if (null spill-out-vars) 0 1)))

         (need-spill? (>= next-n max-n))

         (new-spill-out-vars (if need-spill? used-vars nil))

         (next-used-vars (union (set-difference current-used-vars last-use-vars) declare-vars))
         (current-max-n 
           (+ (max (length current-used-vars) (length next-used-vars))
              (if (null spill-out-vars) 0 1)))
         (current-dup-n 
           (if need-spill?
             (- max-n (length live-vars) (max last-use-vars-n declare-vars-n) 1)
             (- max-n current-max-n)))
         (next-dup-n (max (min current-dup-n dup-room-n) 0))
         (next-dup-vars (if (or (= next-dup-n 0) (= max-n current-max-n)) nil
                          (if need-spill?
                            (union dup-vars used-vars)
                            dup-vars)))
         (next-spill-out-vars (append spill-out-vars new-spill-out-vars)))

    (values 
      (copy-tree
        `(:spill
           (:used ,@next-used-vars)
           (:duplicate ,next-dup-n ,@next-dup-vars)
           (:spill-out ,@next-spill-out-vars)))
      need-spill?)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-spill) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (live-vars-pair (caar env))
        (spill-list (cdar env)))

    (let* ((binds-live-vars (caddr live-vars-pair))
           (binds-env (make-new-env parser env `((:live-vars ,@binds-live-vars)
                                                 (:no-spill))))
           (new-binds (cps-binds parser binds binds-env))
           (new-next-cps (cps-binds parser next-cps env)))

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

    (print `(:cps-bind-spill-status ,func-name ,(car next-cps)))
    ;(print `(:next-live-varsssss ,(cdr live-vars-pair)))

    (let* ((next-live-vars (car (nth 5 live-vars-pair)))
           (next-spill-list (update-next-spill-list parser (cdr live-vars-pair) (cdr spill-list)))
           (bind-env (make-new-env parser env `((:live-vars ,@next-live-vars)
                                                ,next-spill-list)))
           (new-next-cps (cps-parse parser next-cps bind-env)))
      (print `(:next ,(car next-cps)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-spill) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (new-env (make-new-env parser env (cdr env))))

    (print `(:env ,(length (car env)) ,(length expr) ,(car expr)))
    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg new-env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps new-env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))
