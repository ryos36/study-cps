;----------------------------------------------------------------
(in-package :cps-reorder)

;----------------------------------------------------------------
(defclass cps-reorder (cps-parser)
  (()))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-reorder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (let* ((result (do-cps-block-analyzer-cps-bind expr))
           (new-env (make-new-env parser env result))
           (new-next-cps (cps-parse parser next-cps new-env)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-reorder) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr)))
        
    ;(print `(:top-env ,(car env)))
    (if (not (= 0 (length (cdr (car env)))))
      (error "reorder parse error"))

    (let ((new-binds (cps-binds parser binds env)))
      (let* ((result (do-cps-block-analyzer-cps-parse next-cps))
             (new-env (make-new-env parser env result))
             (new-next-cps (cps-parse parser next-cps new-env)))

      `(,fix-op ,new-binds ,new-next-cps)))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-reorder) expr env)
  (let* ((top-env (car env))
         (replace-insn (car top-env)))

    (if (null replace-insn)
      (call-next-method)

      (let* ((replace-expr (cps-expr replace-insn))

             (func-name (cadr expr))
             (args (caddr expr))

             (replace-func-name (cadr replace-expr))
             (replace-args (caddr replace-expr)))

        (setf (car env) (cdr top-env))

        ;(print `(pop-expr ,(name replace-insn)))

        (if (not (= 0 (length (cdr top-env))))
          (error "reorder parse error"))

        `(:APP ,replace-func-name ,replace-args)))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-reorder) expr env)
  (let* ((top-env (car env))
         (replace-insn (car top-env)))

    (if (null replace-insn)
      (call-next-method)

      (let ((replace-expr (cps-expr replace-insn)))

        ;(print `(:cps-primitive ,replace-insn :replace-expr))

        ;(if (null (car replace-insn))
          ;(print `(env ,top-env)))

        (setf (car env) (cdr top-env))

        (let ((op (car replace-expr))
              (args (cadr replace-expr))
              (result (caddr replace-expr))
              (next-cpss (cadddr expr)))

          ;(print `(:op ,op ,args ,result))
          (if (and (not (= (length next-cpss) 1))
                   (not (= 0 (length (cdr (car env))))))
            (error "reorder parse error"))

          (let ((new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

            `(,op ,args ,result ,new-next-cpss)))))))
