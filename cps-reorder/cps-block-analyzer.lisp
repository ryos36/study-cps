;----------------------------------------------------------------
;(load "../k-transfer/cps-parser.lisp")

;----------------------------------------------------------------
(defclass cps-block-analyzer (cps-parser)
  ((new-order
     :initform '()
     :accessor get-new-order
     )))

;----------------------------------------------------------------
(defmethod cps-reset-environment ((parser cps-block-analyzer))
  (setf (get-new-order parser) '()))

;----------------------------------------------------------------
(defmethod pop-cps-expr ((parser cps-block-analyzer))
  (with-slots ((new-order new-order)) parser
    (if (null new-order) (error "No Expr Item."))
    (let ((top-expr (car new-order)))
      (setf new-order (cdr new-order))
      top-expr)))

;----------------------------------------------------------------
(defun init-env ()
  (copy-tree '((:insns )(:vars ))))

;----------------------------------------------------------------
(defmethod eval-insn-value ((parser cps-block-analyzer) op)
  1)

;----------------------------------------------------------------
(defmethod chase-depth ((parser cps-block-analyzer) runnable insn-list var-list)
  (labels ((chase-depth0 (vars-values n m)
           ;(print `(vars-values ,vars-values))
            (if (null vars-values) (values n m)
              (let* ((var-value (car vars-values))
                     (var (car var-value))
                     (value (cdr var-value))
                     (inst-value-list
                        (remove-if #'null
                          (mapcar #'(lambda (inst)
                            (let* ((op (car inst))
                                   (info (cdr inst))
                                   (args (cadr info))
                                   (result (caddr info))
                                   (hit (member var args)))
                              (if hit
                                `(,inst . ,(+ value (eval-insn-value parser op))))))
                                  insn-list)))
                     (new-m (+ m (length inst-value-list)))
                     (add-vars-values-list
                       (mapcar #'(lambda (iv)
                         (let* ((inst (car iv))
                                (v (cdr iv))
                                (rv-syms (cadddr inst))
                                (naive-rv-syms (if (null rv-syms) '(:dummy) rv-syms)))

                           (mapcar #'(lambda(x) (cons x v)) naive-rv-syms)))
                               inst-value-list))
                     (add-vars-values
                       (reduce #'append add-vars-values-list))
                     (maxn
                       (reduce #'(lambda (a b) (max a (cdr b))) add-vars-values :initial-value n)))
                ;(print `(cd-value-inst-list ,maxn ,add-vars-values))
                (chase-depth0 (append (cdr vars-values) add-vars-values) maxn new-m)))))

  (let* ((op (car runnable))
         (info (cdr runnable))
         (stat (car info))
         (args (cadr info))
         (result (caddr info))
         (op-value (eval-insn-value parser op)))

    ;(print `(:info ,info))
    (if (null args)
      op-value
      (chase-depth0 
        (mapcar #'(lambda(x) (cons x op-value)) result)
        op-value 1)))))
;----------------------------------------------------------------
(defmethod cps-select-runnable ((parser cps-block-analyzer) selected-runnable var-list insn-list)

  (setf (cadr selected-runnable) :selected)

  (let* ((insn (cdr selected-runnable))
         (args (cadr insn))
         (result (caddr insn))
         (used-args-pos-list
            (mapcar #'(lambda (arg)
              (remove-if #'null
                (mapcar #'(lambda(i)
                  (let ((stat (cadr i))
                        (i-args (caddr i)))
                    (if (eq :selected stat)
                      nil
                      (car (member arg i-args)))))
                        insn-list)))
                    args))
         (dead-args (remove-if #'null (mapcar #'(lambda (a b) (if (null b) a)) args used-args-pos-list))))
    ;(print `(,args ,used-args-pos-list ,dead-args))

    (if dead-args
      (mapcar #'(lambda (da) (setf (cdr (assoc da var-list)) :dead)) dead-args))

    (if result
      (mapc #'(lambda (r) (setf (cdr (assoc r var-list)) :live)) result))
  ))
;----------------------------------------------------------------
; (op . ( [:init | :runnable | :selected ] (args...) (result) expr ))
; (<var> . [:init | :live | :dead])

(defmethod do-cps-block-analyzer ((parser cps-block-analyzer) env)
  (let* ((top-env (car env))
         (vars-holder (assoc :vars top-env))
         (var-list (cdr vars-holder))
         (insns-holder (assoc :insns top-env))
         (insn-list (cdr insns-holder)))

      ;(print `(env ,env))
      (print `(var-list ,var-list))
      (print `(insn-list ,insn-list))
      (labels ((do-cps-block-analyzer0 (len rv)
        (if (= 0 len) (nreverse rv)
          (let* ((runnable-list
                  (remove-if #'null
                    (mapcar #'(lambda (insn)
                      (let* ((op (car insn))
                             (info (cdr insn))
                             (stat (car info))
                             (args (cadr info))
                             (result (caddr info)))

                        (case stat
                          (:runnable insn)
                          (:selected nil)
                          (:init
                            (if 
                              (reduce #'(lambda (a b) (and a b)) 
                                      (mapcar #'(lambda (x) 
                                        (car (member x var-list :test 
                                           #'(lambda (x0 target) 
                                               (and (eq (car target) x0)
                                                    (eq (cdr target) :live))))))
                                              args))
                              insn)))))
                            insn-list)))
                 (selected-runnable
                   (if (= 1 (length runnable-list))
                     (car runnable-list)
                     (let* ((depth-list (mapcar #'(lambda (runnable) 
                              (multiple-value-bind (n m)
                                (chase-depth parser runnable insn-list var-list)
                                (list n m))) runnable-list))
                            (runnable-depth-list
                              (mapcar #'cons runnable-list depth-list)))

                       (car 
                         (reduce #'(lambda (a b)
                                   (let* ((depth-a (cdr a))
                                          (depth-a0 (car depth-a))
                                          (depth-b (cdr b))
                                          (depth-b0 (car depth-b))
                                          (a<b? 
                                            (cond
                                              ((< depth-a0 depth-b0) t)
                                              ((= depth-a0 depth-b0)
                                               (let ((depth-a1 (cadr depth-a))
                                                     (depth-b1 (cadr depth-b)))
                                                 (< depth-a1 depth-b1)))
                                              (nil nil))))
                                     (if a<b? b a)))
                               runnable-depth-list))))))

            (print `hello)
            (print `(runnable-list ,runnable-list))
            (mapc #'(lambda (insn) 
                      (setf (cadr insn) :runnable)) runnable-list)
            (cps-select-runnable parser selected-runnable var-list insn-list)

            ;(setf (cadr selected-runnable) :selected)
            ;(print `(selected-runnable ,selected-runnable))
            ;(print `(runnable-list ,runnable-list))
            ;(print `(insn-list ,insn-list))
            ;(print `(var-list ,var-list))

            (let ((next-len 
                    (if (null selected-runnable) len (- len 1)))
                  (next-rv
                    (if (null selected-runnable) rv (cons selected-runnable rv))))
              (do-cps-block-analyzer0 next-len next-rv))))))

        (do-cps-block-analyzer0 (length insn-list) '()))))

;----------------------------------------------------------------
(defmethod make-new-env ((parser cps-block-analyzer) env &optional (new-env-item (init-env)))
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
         (insn-list (cdr insns-holder)))

    (setf (cdr insns-holder) (cons `(,key . ,(copy-tree value)) insn-list))))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-block-analyzer) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr)))

    `(:DUMMY-FIX ,(mapcar #'(lambda (bind) (car bind)) binds))))

;----------------------------------------------------------------
(def-cps-func cps-bind-old ((parser cps-block-analyzer) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr))
        (new-env (make-new-env parser env)))

    (mapc #'(lambda(arg) 
                (set-variable arg :live new-env)) args)

    (let ((new-next-cps (cps-parse parser next-cps new-env))
          (result (do-cps-block-analyzer parser new-env)))

      (setf (get-new-order parser) result)

      ;(print `(result ,result))

      `(,func-name ,args ,new-next-cps))))

(def-cps-func cps-bind ((parser cps-block-analyzer) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (mapc #'(lambda(arg) 
                (set-variable arg :live env)) args)

    (let ((new-next-cps (cps-parse parser next-cps env)))
      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-block-analyzer) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr)))
        
    (let ((new-func-name (cps-symbol parser func-name env))
          (new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) (copy-tree args)))

          )
      (set-instruction :app `(:init (,func-name ,@args) nil ,expr) env)
      `(:APP ,new-func-name ,new-args))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-block-analyzer) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (top-env (car env)))

    (print `(op ,op))
    (mapc #'(lambda (r) (set-variable r :init env)) result)
    ; ignore (:label |x|) and number ex. 1
    (set-instruction op `(:init ,(remove-if #'(lambda (x) (not (cps-symbolp x))) args) ,result ,expr) env)

    (let ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
          (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

      `(,op ,new-args ,result ,new-next-cpss))))

;----------------------------------------------------------------
(let* ((analyzer (make-instance 'cps-block-analyzer))
       (env (make-new-env analyzer '())))

  (defun do-cps-block-analyzer-cps-bind (expr)
    (cps-reset-environment analyzer)
    (let ((new-env (make-new-env analyzer env )))
      (cps-bind analyzer expr new-env)
      (do-cps-block-analyzer analyzer new-env)))

  (defun do-cps-block-analyzer-cps-parse (expr)
    (cps-reset-environment analyzer)
    (let ((new-env (make-new-env analyzer env)))
      (cps-parse analyzer expr new-env)
      (do-cps-block-analyzer analyzer new-env))))
