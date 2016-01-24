;----------------------------------------------------------------
(in-package :cps-reorder)

;----------------------------------------------------------------
(defclass cps-block-analyzer (cps-parser)
  ((scheduler :initarg :scheduler :initform (make-instance 'vm-scheduler) :accessor scheduler)))

;----------------------------------------------------------------
(defmethod cps-reset-environment ((parser cps-block-analyzer))
  ;(setf (get-new-order parser) '())
  )

;----------------------------------------------------------------
(defun init-env ()
  (copy-tree '((:insns )(:vars ))))

;----------------------------------------------------------------
(defmethod make-new-env ((parser cps-block-analyzer) env &optional (new-env-item (init-env)))
  (cons new-env-item env))

;----------------------------------------------------------------
(defgeneric add-register (parser sym &optional stat))

(defmethod add-register ((parser cps-block-analyzer) sym &optional (stat :init))
    (let ((reg (make-instance 'resource :name sym :status stat)))
      (add-resource (scheduler parser) reg)))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-block-analyzer) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr)))

    `(:NEXT-BLOCK ,fix-op ,(mapcar #'(lambda (bind) (car bind)) binds))))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-block-analyzer) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (mapc #'(lambda(arg) 
                (add-register parser arg :activate)) args)

    (let ((new-next-cps (cps-parse parser next-cps env)))
      `(,func-name ,args ,new-next-cps))))

;----------------------------------------------------------------
(def-cps-func cps-app ((parser cps-block-analyzer) expr env)
  (let ((func-name (cadr expr))
        (args (caddr expr))
        (scheduler (scheduler parser)))
        
    (let ((new-func-name (cps-symbol parser func-name env))
          (new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) (copy-tree args))))

      (add-apply-instruction scheduler expr func-name args)
      `(:APP ,new-func-name ,new-args))))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser cps-block-analyzer) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr))
        (top-env (car env))
        (scheduler (scheduler parser)))

    ; ignore (:label |x|) and number ex. 1
    (add-primitive-instruction scheduler expr  op args result)

   ;(= (length next-cpss) 1)
    (if (not (null result))
      (let* ((new-args (mapcar #'(lambda (arg) (cps-terminal parser arg env)) args))
            (new-next-cpss (mapcar #'(lambda (cps) (cps-parse parser cps env)) next-cpss)))

        `(,op ,new-args ,result ,new-next-cpss))

      (if (null next-cpss)
        '(:NO-BLOCK)
        `(:NEXT-BLOCK ,op ,next-cpss)))))

;----------------------------------------------------------------
(defmethod do-cps-block-analyzer ((parser cps-block-analyzer) env)
  (let ((scheduler (scheduler parser)))
    (build-connection scheduler)
    (initialize-activate-resources scheduler)
    (initialize-ready-nodes scheduler)

    (labels ((do-cps-block-analyzer0 (n)
              ;(print `(:do-cps-block-analyzer-cps-bind ,n ,(is-finished? scheduler)))
               (if (is-finished? scheduler)
                 (ordered-nodes scheduler)
                 (let ((node (select-candidate-node-to-ready scheduler)))
                   (if node 
                     (apply-node scheduler node))

                   (update-accounting scheduler)
                   (update-ready-nodes scheduler)
                   (do-cps-block-analyzer0 (+ n 1))))))

      (let ((rv (do-cps-block-analyzer0 0)))
        ;(print `(:result ,rv))
        rv))))

;----------------------------------------------------------------
(defun do-cps-block-analyzer-cps-bind (expr)
  (let* ((analyzer (make-instance 'cps-block-analyzer))
         (env (make-new-env analyzer '())))
    (cps-reset-environment analyzer)
    (let ((new-env (make-new-env analyzer env)))
      (cps-bind analyzer expr new-env)
      (do-cps-block-analyzer analyzer new-env))))

;----------------------------------------------------------------
(defun do-cps-block-analyzer-cps-parse (expr)
  (let* ((analyzer (make-instance 'cps-block-analyzer))
         (env (make-new-env analyzer '())))
    (cps-reset-environment analyzer)
    (let ((new-env (make-new-env analyzer env)))
      (cps-parse analyzer expr new-env)
      (do-cps-block-analyzer analyzer new-env))))
