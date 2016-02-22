;----------------------------------------------------------------
(in-package :sinby.csf.acps-to-acps)

;----------------------------------------------------------------
(defclass acps-to-acps () 
  ((sym-no :initform 0 :initarg :sym-no)
   (sym-name :initform "sym" :initarg :sym-name)))

;----------------------------------------------------------------
(defmethod acps-error-exit ((converter acps-to-acps) expr (env acps-environment))
   (error 'parse-error :expr expr :env env))

;----------------------------------------------------------------
(defmethod acps-gensym ((converter acps-to-acps) &optional is-label)
  (let ((rv (intern (format nil "~a~a~a" (if is-label ":" "")
                            (slot-value converter 'sym-name)
                            (slot-value converter 'sym-no)))))
    (incf (slot-value converter 'sym-no))
    rv))

;----------------------------------------------------------------
(defun terminal-p (expr)
  (or 
    (eq :#t expr)
    (eq :#f expr)
    (eq :unspecified expr)
    (null expr)
    (symbolp expr)
    (numberp expr)))

;----------------------------------------------------------------
(defun acps-symbolp (expr)
  (not (or 
         (eq :#t expr)
         (eq :#f expr)
         (eq :unspecified expr)
         (null expr)
         (numberp expr)
         (and (listp expr) 
              (let ((tag-sym (car expr)))
                (or (eq tag-sym :label)
                    (eq tag-sym :address)))))))

;----------------------------------------------------------------
(defmethod acps-primitive-p ((converter acps-to-acps) op)
  (case op 
    (:#t nil)
    (:#f nil)

    (:+  t)
    (:-  t)
    (:*  t)

    (:>> t)
    (:<< t)

    (:<  t)
    (:>  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)

    (:heap t)
    (:record-set! t)
    (:record-ref t)
    (:record-offs t)
    (:stack t)
    (:pop t)

    (otherwise nil)))

;----------------------------------------------------------------
(defun compare-primitivep (sym)
  (case sym
    (:>  t)
    (:<  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)
    (otherwise nil)))

;----------------------------------------------------------------
(defmethod acps-terminal ((converter acps-to-acps) expr (env acps-environment))
  (cond
    ((eq :#t expr) :#t)
    ((eq :#f expr) :#f)
    ((eq :unspecified expr) :unspecified)
    ((null expr) nil)
    ((symbolp expr) (acps-symbol converter expr env))
    (t expr)))

;----------------------------------------------------------------
(defmethod acps-symbol ((converter acps-to-acps) expr (env acps-environment))
  expr)

;----------------------------------------------------------------
(defmethod acps-bind ((converter acps-to-acps) expr (env acps-environment))
  (let ((func-name (car expr))
        (attr (cadr expr))
        (args (caddr expr))
        (next-acps (cadddr expr)))
    (let ((new-next-acps (acps-parse converter next-acps env)))

      `(,func-name ,attr ,args ,new-next-acps))))

;----------------------------------------------------------------
(defmethod acps-binds ((converter acps-to-acps) binds (env acps-environment))
  (mapcar #'(lambda (bind) (acps-bind converter bind env)) binds))

;----------------------------------------------------------------
(defmethod acps-fix ((converter acps-to-acps) expr (env acps-environment))
  (let ((fix-op (car expr))
        (attr (cadr expr))
        (binds (caddr expr))
        (next-acps (cadddr expr)))
        
    (let ((new-binds (acps-binds converter binds env))
          (new-next-acps (acps-parse converter next-acps env)))

      `(,fix-op ,attr ,new-binds ,new-next-acps))))

;----------------------------------------------------------------
(defmethod acps-fixh ((converter acps-to-acps) expr (env acps-environment))
  (acps-fix converter expr env))

;----------------------------------------------------------------
(defmethod acps-fixs ((converter acps-to-acps) expr (env acps-environment))
  (acps-fix converter expr env))

;----------------------------------------------------------------
(defmethod acps-appf ((converter acps-to-acps) expr (env acps-environment))
  (let ((attr (cadr expr)
        (func-name (caddr expr))
        (args (cadddr expr))))
        
    (let ((new-func-name (acps-symbol converter func-name env))
          (new-args (mapcar #'(lambda (arg) (acps-terminal converter arg env)) args)))
      `(:APPF ,attr ,new-func-name ,new-args))))

;----------------------------------------------------------------
(defmethod acps-appb ((converter acps-to-acps) expr (env acps-environment))
  (let ((attr (cadr expr)
        (func-name (caddr expr))
        (args (cadddr expr))))
        
    (let ((new-func-name (acps-symbol converter func-name env))
          (new-args (mapcar #'(lambda (arg) (acps-terminal converter arg env)) args)))
      `(:APPB ,attr ,new-func-name ,new-args))))

;----------------------------------------------------------------
(defmethod acps-exit ((converter acps-to-acps) expr (env acps-environment))
  (let ((attr (cadr expr))
        (arg0 (caaddr expr)))
    (let ((new-arg0 (acps-terminal converter arg0 env)))
      `(:EXIT ,attr (,new-arg0) () ()))))

;----------------------------------------------------------------
(defmethod acps-primitive ((converter acps-to-acps) expr (env acps-environment))
  (let ((op (car expr))
        (attr (cadr expr))
        (args (caddr expr))
        (result (cadddr expr))
        (next-acpss (car (cddddr expr))))

    (let ((new-args (mapcar #'(lambda (arg) (acps-terminal converter arg env)) args))
          (new-next-acpss (mapcar #'(lambda (acps) (acps-parse converter acps env)) next-acpss)))

      `(,op ,attr ,new-args ,result ,new-next-acpss))))

;----------------------------------------------------------------
(defmethod acps->acps ((converter acps-to-acps) expr (env acps-environment))
  (if (terminal-p expr)
    (acps-terminal converter expr env)
    (let ((op (car expr)))
      (case op
        (:fixh (acps-fixh converter expr env))
        (:fixs (acps-fixs converter expr env))
        (:appf (acps-appf converter expr env))
        (:appb (acps-appb converter expr env))

        ;(:fixg )
        ;(:id )

        (:exit (acps-exit converter expr env))
        (otherwise (if (acps-primitive-p converter op)
                     (acps-primitive converter expr env)
                     (acps-error-exit converter expr env)))))))
