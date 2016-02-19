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
    (if (not (assoc key top-env))
      (setf (car env) (cons `(,key . ,value) top-env)))))

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
        (next-cps (caddr expr)))

    ;(print `(cps-bind ,func-name ,expr))
    (mapc #'(lambda(arg) 
                (set-variable parser arg t env)) args)

    (set-variable parser func-name t env)
    ;(print `(:add-func-name ,func-name))

    ;(print `(:next-cps ,(car next-cps)))
    (let ((new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-binds ((parser free-variable-finder) binds env)
  (let* ((vars-list '())
         (new-expr-cps '()))

    #|
    (let ((func-names (mapcar #'(lambda (x) (car x)) binds)))
      (mapc #'(lambda(arg) 
                (set-variable parser arg nil env)) func-names))
    |#

    (mapcar #'(lambda (bind) 
                (let ((new-env (make-new-env parser env)))
                  (push (cps-bind parser bind new-env) new-expr-cps)
                  (setf vars-list
                        (append vars-list
                                (car new-env)))))
            binds)

    (mapc #'(lambda(arg) 
              (set-variable parser arg nil env))
          (filter-free-variables vars-list))

    (nreverse new-expr-cps)))
  
;----------------------------------------------------------------
(def-cps-func cps-fix ((parser free-variable-finder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (binds-env (make-new-env parser env))
        (next-cps-env (make-new-env parser env)))

    (let ((func-names (mapcar #'(lambda (x) (car x)) binds)))
      (mapc #'(lambda(arg) 
                (set-variable parser arg t next-cps-env)) func-names)
        
      (let ((new-binds (cps-binds parser binds binds-env))
            (new-next-cps (cps-parse parser next-cps next-cps-env)))

        (mapc #'(lambda(arg) 
              (set-variable parser arg (find arg func-names) env))
              (filter-free-variables (car binds-env)))

        (mapc #'(lambda(arg) 
              (set-variable parser arg nil env))
              (filter-free-variables (car next-cps-env)))

        `(,fix-op ,new-binds ,new-next-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-exit ((parser free-variable-finder) expr env)
  (let ((arg0 (caadr expr)))

    (set-variable parser arg0 nil env)

    (let ((new-arg0 (cps-terminal parser arg0 env)))
      `(:EXIT (,new-arg0) () ()))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser free-variable-finder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (mapc #'(lambda (r) (set-variable parser r t env)) result)


    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss '()))

      (if (= (length next-cpss) 1) 
        (setf new-next-cpss (list (cps-parse parser (car next-cpss) env)))

        (let ((vars-list '()))
          (mapcar #'(lambda (cps) 
                      (let ((new-env (make-new-env parser env)))
                        (push (cps-parse parser cps new-env) new-next-cpss)
                        (setf vars-list
                              (append vars-list
                                      (car new-env))))) next-cpss)
          (mapc #'(lambda(arg) 
                    (set-variable parser arg nil env))
                (filter-free-variables vars-list))))

      `(,op ,new-args ,result ,(nreverse new-next-cpss)))))

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
