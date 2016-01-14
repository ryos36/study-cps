;----------------------------------------------------------------
;(load "../k-transfer/cps-parser.lisp")

;----------------------------------------------------------------
(defclass cps-reorder (cps-parser)
  ((new-order
     :initform '()
     :accessor get-new-order
     )))

;----------------------------------------------------------------
(defmethod reset ((parser cps-reorder))
  (setf (get-new-order parser) '()))

;----------------------------------------------------------------
(defmethod pop-cps-expr ((parser cps-reorder))
  (with-slots ((new-order new-order)) parser
    (if (null new-order) (error "No Expr Item."))
    (let ((top-expr (car new-order)))
      (setf new-order (cdr new-order))
      top-expr)))

;----------------------------------------------------------------
(defmethod make-new-env ((parser cps-reorder) env &optional (new-env-item (init-env)))
  (cons new-env-item env))

;;----------------------------------------------------------------
;(def-cps-func cps-fix ((parser cps-reorder) expr env)
;  (let ((fix-op (car expr))
;        (binds (cadr expr))
;        (next-cps (caddr expr))
;        (new-env (make-new-env parser env)))
;        
;    (let ((new-binds (cps-binds parser binds env))
;          (new-next-cps (cps-parse parser next-cps env)))
;
;      `(,fix-op ,new-binds ,new-next-cps))))
;
;;----------------------------------------------------------------
;(def-cps-func cps-bind ((parser cps-reorder) expr env)
;  (let ((func-name (car expr))
;        (args (cadr expr))
;        (next-cps (caddr expr))
;        (new-env (make-new-env parser env)))
;
;    (mapc #'(lambda(arg) 
;                (set-variable arg :live new-env)) args)
;
;    (let ((new-next-cps (cps-parse parser next-cps new-env)))
;      (cps-do-reorder parser new-env)
;
;      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-reorder) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))
        (replace-insn (pop-cps-expr parser)))

    (let* ((replace-expr (nth 4 replace-insn))

           (replace-func-name (cadr replace-expr))
           (replace-args (caddr replace-expr)))

      (print `(pop-expr ,replace-expr))

      (if (not (= 0 (length (get-new-order parser))))
        (error `("reorder parse error" ,(get-new-order parser))))

      `(:APP ,replace-func-name ,replace-args))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-reorder) expr env)
  (let* ((replace-insn (pop-cps-expr parser))
         (replace-expr (nth 4 replace-insn)))
    (print `(pop-expr ,replace-expr))

    (let ((op (car replace-expr))
          (args (cadr replace-expr))
          (result (caddr replace-expr))
          (next-cpss (cadddr expr))
          (top-env (car env)))

      (let ((new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,args ,result ,new-next-cpss)))))

