;----------------------------------------------------------------
(in-package :sinby.csf.acps-free-variables)

;----------------------------------------------------------------
(defclass acps-free-variables (acps-to-acps) 
  ())

;----------------------------------------------------------------
(defmethod make-new-env-add-variables ((converter acps-to-acps) (env acps-environment) decl-vars used-vars)
  (let ((new-env (make-new-environment env)))
    (add-infomation new-env converter :decl-vars decl-vars)
    (add-infomation new-env converter :used-vars used-vars)))


;----------------------------------------------------------------
(defmethod pickup-used-vars ((converter acps-to-acps) (env acps-environment))
  `(a b c))

;----------------------------------------------------------------
(defmethod get-heap-size ((converter acps-to-acps) (env acps-environment))
  5)

;----------------------------------------------------------------
(defmethod acps-fix ((converter acps-to-acps) expr (env acps-environment))
  (let ((fix-op (car expr))
        (attr (cadr expr))
        (binds (caddr expr))
        (next-acps (cadddr expr)))
        
    (let* ((new-env-for-binds (make-new-environment env))
           (new-binds (acps-binds converter binds new-env-for-binds))
           (func-names (mapcar #'(lambda (bind) (car bind)) binds))
           (new-env-for-next-acps (make-new-env-add-variables converter env func-names nil))
           (new-next-acps (acps->acps converter next-acps new-env-for-next-acps))
           (used-vars-in-funcs (pickup-used-vars converter new-env-for-bind))
           (heap-size (+ (get-heap-size converter new-env-for-bind)
                         (get-heap-size converter new-env-for-next-acps))))

      `(,fix-op ,attr ,new-binds ,new-next-acps))))
