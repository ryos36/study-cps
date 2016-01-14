;----------------------------------------------------------------
(load "../k-transfer/cps-parser.lisp")
(load "cps-block-analyzer.lisp")

;----------------------------------------------------------------
(defclass cps-reorder (cps-parser)
  (()))

;----------------------------------------------------------------
;(defmethod make-new-env ((parser cps-reorder) env &optional (new-env-item (init-env)))
;  (cons new-env-item env))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-reorder) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (analyzer (make-instance 'cps-block-analyzer)))

    (cps-bind analyzer expr (make-new-env analyzer '()))

    (let* ((result0 (get-new-order analyzer))
           (result (do-cps-block-analyzer-cps-bind expr))
           (new-env (make-new-env parser env result))
           ;(x (print `(xxresult ,env ,(car new-env))))
           (new-next-cps (cps-parse parser next-cps new-env)))

      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
;(def-cps-func cps-fix ((parser cps-reorder) expr env)
;  (let ((fix-op (car expr))
;        (binds (cadr expr))
;        (next-cps (caddr expr))
;        (new-env (make-new-env env)))
;        
;    ; stop reorder
;    (let ((new-binds (cps-binds parser binds new-env))
;          (new-next-cps (cps-parse parser next-cps env)))
;
;      `(,fix-op ,new-binds ,new-next-cps)))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-reorder) expr env)
  (let* ((top-env (car env))
         (replace-insn (car top-env)))
    (if (null replace-insn)
      (call-next-method)

      (let* ((replace-expr (nth 4 replace-insn))

             (func-name (cadr expr))
             (args (caddr expr))

             (replace-func-name (cadr replace-expr))
             (replace-args (caddr replace-expr)))

        (setf (car env) (cdr top-env))

        ;(print `(pop-expr ,(car replace-insn)))

        ;(print `(top-env ,top-env))
        (if (not (= 0 (length (cdr top-env))))
          (error "reorder parse error"))

        `(:APP ,replace-func-name ,replace-args)))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-reorder) expr env)
  (let* ((top-env (car env))
         (replace-insn (car top-env)))

    (if (null replace-insn)
      (call-next-method)

      (let ((replace-expr (nth 4 replace-insn)))

        ;(print `(replace-insn ,(car replace-insn)))

        ;(if (null (car replace-insn))
          ;(print `(env ,top-env)))
        (setf (car env) (cdr top-env))

        (let ((op (car replace-expr))
              (args (cadr replace-expr))
              (result (caddr replace-expr))
              (next-cpss (cadddr expr))
              (top-env (car env)))

          (let ((new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

            `(,op ,args ,result ,new-next-cpss)))))))

