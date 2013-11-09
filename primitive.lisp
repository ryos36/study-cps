;----------------------------------------------------------------
; primitive

(defmacro primitive-2 (expr env &rest body)
  `(let ((arg0 (parse-mini-scheme (cadr ,expr) ,env))
         (arg1 (parse-mini-scheme (caddr ,expr) ,env))
         (env ,env))
     ,@body))


#|
(defun --two (expr env) (- 
                          (parse-mini-scheme (cadr expr) env)
                          (parse-mini-scheme (caddr expr) env)))
|#

(defun +-two (expr env)
  (primitive-2 expr env 
               (+ arg0 arg1)))

(defun --two (expr env)
  (primitive-2 expr env 
               (- arg0 arg1)))

(defun >>-two (expr env) 
  (primitive-2 expr env
               (labels ((>> (x r)
                            (if (= r 0) x
                              (>> (floor (/ x 2)) (- r 1)))))
                 (>> arg0 arg1))))

(defun <<-two (expr env) 
  (primitive-2 expr env
               (labels ((<< (x r)
                            (if (= r 0) x
                              (<< (* x 2) (- r 1)))))
                 (<< arg0 arg1))))

(defun >-two (expr env) 
  (primitive-2 expr env
               (if (> arg0 arg1) :#t :#f)))

(defun <-two (expr env) 
  (primitive-2 expr env
               (if (< arg0 arg1) :#t :#f)))

(defun >=-two (expr env) 
  (primitive-2 expr env
               (if (>= arg0 arg1) :#t :#f)))

(defun <=-two (expr env) 
  (primitive-2 expr env
               (if (<= arg0 arg1) :#t :#f)))

(defun =-two (expr env) 
  (primitive-2 expr env
               (if (= arg0 arg1) :#t :#f)))

; heap-record is '(1 2 3 ... )
(defun heap (expr env)
  (mapcar #'(lambda (x) (parse-mini-scheme x env)) (cdr expr)))

(defun record-ref (expr env)
  (let ((heap-record (parse-mini-scheme (cadr expr) env))
        (pos (parse-mini-scheme (caddr expr) env)))
    (nth pos heap-record)))

(defun record-set! (expr env)
  (let ((heap-record (parse-mini-scheme (cadr expr) env))
        (pos (parse-mini-scheme (caddr expr) env))
        (value (parse-mini-scheme (cadddr expr) env)))
    (setf (nth pos heap-record) value)
    heap-record))
