;----------------------------------------------------------------
(load "../k-transfer/cps-parser.lisp")

;----------------------------------------------------------------
(defclass cps-reoder (cps-parser)
  ())

;----------------------------------------------------------------
(defun init-env ()
  (copy-tree '((:insns )(:vars ))))

;----------------------------------------------------------------
; (op . ( [:init | :runnable | :dead] (args...) (result)))
; (<var> . [:init | :live | :dead])

(defmethod cps-do-reoder ((parser cps-reoder) env)
  (let* ((top-env (car env))
         (vars-holder (assoc :vars top-env))
         (var-list (cdr (cdr vars-holder)))
         (insns-holder (assoc :insns top-env))
         (insns-list (cdr (cdr insns-holder))))

      ;(print `(env ,env))
      (print `(var-list ,var-list))
      ;(print `(insns-list ,insns-list))
      (let ((runnable-list
              (remove-if #'null
                (mapcar #'(lambda (insn)
                  (let* ((op (car insn))
                         (info (cdr insn))
                         (stat (car info))
                         (args (cadr info))
                         (result (cadr info)))

                    (if 
                      (reduce #'(lambda (a b) (and a b)) 
                        (mapcar #'(lambda (x) 
                            (car (member x var-list :test 
                               #'(lambda (x0 target) 
                                   (and (eq (car target) x0)
                                        (eq (cdr target) :live))))))
                                args))
                      insn)))

                        insns-list))))
        (print `(runnable-list ,runnable-list)))

       ;dolist (insn (cdr insns-holder))

    ))

;----------------------------------------------------------------
(defmethod make-new-env ((parser cps-reoder) env &optional (new-env-item (init-env)))
  (cons new-env-item env))

;----------------------------------------------------------------
(defun set-variable (key value env)
  (let* ((top-env (car env))
         (vars-holder (assoc :vars top-env))
         (vars-list (cdr vars-holder)))

    (setf (cdr vars-holder) (cons `(,key . ,value) vars-list))))

;----------------------------------------------------------------
(defun set-instruction (key value env)
  (let* ((top-env (car env))
         (insns-holder (assoc :insns top-env))
         (insns-list (cdr insns-holder)))

    (setf (cdr insns-holder) (cons `(,key . ,(copy-tree value)) insns-list))))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-reoder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))
        (new-env (make-new-env parser env)))
        
    (let ((new-binds (cps-binds parser binds env))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,fix-op ,new-binds ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-reoder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (new-env (make-new-env parser env)))

    (mapc #'(lambda(arg) 
                (set-variable arg :live new-env)) args)

    (let ((new-next-cps (cps-parse parser next-cps new-env)))
      (cps-do-reoder parser new-env)

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-reoder) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))
        
    (let ((new-func-name (cps-symbol parser func-name env))
          (new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args)))
      (set-instruction :app `(:init (,func-name ,@args) nil) env)
      `(:APP ,new-func-name ,new-args))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-reoder) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (top-env (car env)))

    (mapc #'(lambda (r) (set-variable r :init env)) result)
    (set-instruction op `(:init ,(remove-if #'(lambda (x) (not (symbolp x))) args) ,result) env)

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

