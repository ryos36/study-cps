;----------------------------------------------------------------
(in-package :sinby.cps.free-variable-finder)

;----------------------------------------------------------------
(defclass free-variable-finder (cps-parser)
  ((suppress-cps-fixs :initarg :suppress-cps-fixs :initform nil :reader suppress-cps-fixs?)))

;----------------------------------------------------------------
(defmethod find-variable ((parser free-variable-finder) expr env)
  (if (null env)
    nil
    (let ((top-env (car env)))
      (if (atom top-env)
        nil
        (let ((key-value (assoc expr top-env)))
          (if key-value key-value
            (find-variable parser expr (cdr env))))))))

;----------------------------------------------------------------
(defmethod set-variable ((parser free-variable-finder) key value env)
  (let ((top-env (car env)))
    (setf (car env) (cons `(,key . ,value) top-env))))

;----------------------------------------------------------------
(def-cps-func cps-symbol ((parser free-variable-finder) expr env)
  (let ((variable-key-value (find-variable parser expr env)))
    (if (null variable-key-value)
      (progn
        (set-variable parser expr nil env)
        `(:free-variable ,expr))
      expr)))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser free-variable-finder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (new-env (make-new-env parser env)))

    ;(print `(cps-bind ,func-name))
    (mapc #'(lambda(arg) 
                (set-variable parser arg t new-env)) args)
    (set-variable parser func-name t new-env)

    ;(print `(next-cps ,(car next-cps)))
    (let ((new-next-cps (cps-parse parser next-cps new-env)))
      ;(print `(next-cps ,(car next-cps)))
       (mapc #'(lambda(arg) 
                 (set-variable parser arg nil env))
             (nreverse (filter-free-variables (car new-env))))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
; call from cps-fixh of cps-parser or
;      from cps-fixs of this class
(def-cps-func cps-fix ((parser free-variable-finder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (bind-env env)
        (next-cps-env (make-new-env parser env)))

    (let ((func-names (mapcar #'(lambda (x) (car x)) binds)))
      (mapc #'(lambda(arg) 
                (set-variable parser arg t next-cps-env)) func-names)
        
      (let ((new-binds (cps-binds parser binds bind-env))
            (new-next-cps (cps-parse parser next-cps next-cps-env)))

        `(,fix-op ,new-binds ,new-next-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-fixs ((parser free-variable-finder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr)))

    (if (suppress-cps-fixs? parser) 
      :SUPPRESS-CPS-FIXS
        
      (let ((func-names (mapcar #'(lambda (x) (car x)) binds)))
        (mapc #'(lambda(arg) 
                  (set-variable parser arg t env)) func-names)

        (let ((new-binds (cps-binds parser binds env))
              (new-next-cps (cps-parse parser next-cps env)))

          `(,fix-op ,new-binds ,new-next-cps))))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser free-variable-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (mapc #'(lambda (r) (set-variable parser r t env)) result)

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun filter-free-variables (all-variables)
  (remove-if #'null (mapcar #'(lambda (x) (if (null (cdr x)) (car x))) all-variables)))

;----------------------------------------------------------------
(defun get-free-variables (finder cps-func expr)
  (let ((finder-env (make-new-env finder '())))
    (funcall cps-func finder expr finder-env)
    (let* ((all-variables (car finder-env))
           (free-variables (filter-free-variables all-variables)))
      free-variables)))
