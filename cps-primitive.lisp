;----------------------------------------------------------------
; cps primitive
;
#|
(defun cps-+ (expr env)
  (let ((args (cadr expr))
        (rv (caaddr expr))
        (next-expr (cadddr expr)))

    (let ((arg0 (parse-expr-terminal (car args) env))
          (arg1 (parse-expr-terminal (cadr args) env))
          (new-env (make-new-env env)))
      (set-key-value rv (+ arg0 arg1) new-env)
      (parse-cps next-expr new-env))))
|#

;----------------------------------------------------------------
(defmacro cps-primitive2 (expr env &rest body)
  `(let ((args (cadr ,expr))
         (rv (caaddr ,expr))
         (next-expr (cadddr ,expr)))

     (let ((arg0 (parse-expr-terminal (car args) ,env))
           (arg1 (parse-expr-terminal (cadr args) ,env))
           (new-env (make-new-env ,env)))
       (set-key-value rv ,@body new-env)
       (parse-cps next-expr new-env))))


(defun cps-+ (expr env)
  (cps-primitive2 expr env (+ arg0 arg1)))

(defun cps-- (expr env)
  (cps-primitive2 expr env (- arg0 arg1)))


(defun cps->> (expr env)
  (cps-primitive2 expr env
                  (labels ((>> (x r)
                               (if (= r 0) x
                                 (>> (floor (/ x 2)) (- r 1)))))
                    (>> arg0 arg1))))

(defun cps-<< (expr env)
  (cps-primitive2 expr env
                  (labels ((<< (x r)
                               (if (= r 0) x
                                 (<< (floor (/ x 2)) (- r 1)))))
                    (<< arg0 arg1))))

(defun cps-< (expr env)
  (cps-primitive2 expr env (< arg0 arg1)))

(defun cps-> (expr env)
  (cps-primitive2 expr env (> arg0 arg1)))

(defun cps-<= (expr env)
  (cps-primitive2 expr env (<= arg0 arg1)))

(defun cps->= (expr env)
  (cps-primitive2 expr env (>= arg0 arg1)))

(defun cps-= (expr env)
  (cps-primitive2 expr env (= arg0 arg1)))

(defun cps-heap (expr env)
  (let ((args (cadr expr))
        (rv (caaddr expr))
        (next-expr (cadddr expr)))

    (let ((new-env (make-new-env env)))
      (set-key-value rv 
                     (mapcar #'(lambda (x) (parse-expr-terminal x env)) args)
                     new-env)
      (parse-cps next-expr new-env))))

(defun cps-record-set! (expr env)
  (let ((args (cadr expr))
        (rv (caaddr expr))
        (next-expr (cadddr expr)))

    (let ((list (parse-expr-terminal (car args) env))
          (pos (parse-expr-terminal (cadr args) env))
          (value (parse-expr-terminal (caddr args) env)))

      (setf (nth pos list) value)
      (parse-cps next-expr env))))

(defun cps-record-ref (expr env)
  (cps-primitive2 expr env 
                  (let ((list arg0)
                        (pos arg1))
                    (nth pos list))))

(defun cps-stack (expr env)
  (let ((args (cadr expr))
        (rv (caaddr expr))
        (next-expr (cadddr expr)))

    (let ((new-env (make-new-env env)))

      (parse-cps next-expr new-env))))

(defun cps-pop (expr env))
