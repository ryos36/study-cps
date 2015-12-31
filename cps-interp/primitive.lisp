;----------------------------------------------------------------
; cps primitive
;

; NOTE : the parsed-arg0 and parsed-arg1 is not replace in macro
;        You can improve this scheme.
;----------------------------------------------------------------
;(op (arg0 arg1) (rv) (next-expr))

(defmacro make-two-args-primitive (func-name &rest body)
  `(defun ,func-name (expr env)
     (let ((args (cadr expr))
           (rv (caaddr expr))
           (cps-pair (cadddr expr)))

       (let ((parsed-arg0 (parse-expr-terminal (car args) env))
             (parsed-arg1 (parse-expr-terminal (cadr args) env))
             (next-cps-expr (car cps-pair))
             (new-env (make-new-env env)))

         (set-key-value rv ,@body new-env)
         (parse-cps next-cps-expr new-env)))))

;----------------------------------------------------------------
(make-two-args-primitive cps-+ (+ parsed-arg0 parsed-arg1))
(make-two-args-primitive cps-- (- parsed-arg0 parsed-arg1))
(make-two-args-primitive cps-* (* parsed-arg0 parsed-arg1))

(make-two-args-primitive cps->> 
                         (labels ((>> (x r)
                                      (if (= r 0) x
                                        (>> (floor (/ x 2)) (- r 1)))))
                           (>> parsed-arg0 parsed-arg1)))

(make-two-args-primitive cps-<< 
                         (labels ((<< (x r)
                                      (if (= r 0) x
                                        (<< (floor (* x 2)) (- r 1)))))
                           (<< parsed-arg0 parsed-arg1)))

;----------------------------------------------------------------
(defmacro make-two-compare-primitive (func-name &rest body)
  `(defun ,func-name (expr env)
     (let ((args (cadr expr))
           (rv (caaddr expr))
           (cps-pair (cadddr expr)))

       (let ((parsed-arg0 (parse-expr-terminal (car args) env))
             (parsed-arg1 (parse-expr-terminal (cadr args) env))
             (next-cps-expr0 (car cps-pair))
             (next-cps-expr1 (cadr cps-pair))
             (new-env (make-new-env env)))

         (let ((compare-rv ,@body))
           (if compare-rv
             (parse-cps next-cps-expr0 new-env)
             (parse-cps next-cps-expr1 new-env)))))))

;----------------------------------------------------------------
(make-two-compare-primitive cps-< 
                            (< parsed-arg0 parsed-arg1))

(make-two-compare-primitive cps->
                            (> parsed-arg0 parsed-arg1))

(make-two-compare-primitive cps-<=
                            (<= parsed-arg0 parsed-arg1))

(make-two-compare-primitive cps->=
                            (>= parsed-arg0 parsed-arg1))

(make-two-compare-primitive cps-=
                            (= parsed-arg0 parsed-arg1))

(make-two-compare-primitive cps-/=
                            (/= parsed-arg0 parsed-arg1))

;----------------------------------------------------------------
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
      (set-key-value rv 
                     (mapcar #'(lambda (x) (parse-expr-terminal x env)) args))
      (push :delimitor *cps-stack*)
      (setf *cps-stack* (nconc rv *cps-stack*))
      (parse-cps next-expr new-env))))

(defun cps-pop (expr env)
  (let ((pop-n (caadr expr))
        (next-expr (cadddr expr)))
    (if (null *cps-stack)
      (cps-error-exit expr env))
    (let ((new-stack (nthcdr pop-n *cps-stack*)))
      (if (not (eq :delimitor (pop new-stack)))
        (cps-error-exit expr env)))
    (parse-cps next-expr env)))

;----------------------------------------------------------------  

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
         (next-expr (caadddr ,expr)))

     (let ((arg0 (parse-expr-terminal (car args) ,env))
           (arg1 (parse-expr-terminal (cadr args) ,env))
           (new-env (make-new-env ,env)))
       (print `(:cps-primitive2 ,,@body ,expr))
       (set-key-value rv ,@body new-env)
       (parse-cps next-expr new-env))))

#|
(defun old-cps-+ (expr env)
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
|#

;----------------------------------------------------------------
#|

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
|#
