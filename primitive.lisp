;----------------------------------------------------------------
; primitive

(defmacro primitive-2 (expr env &rest body)
  `(let ((arg0 (parse-mini-scheme (cadr ,expr) ,env))
         (arg1 (parse-mini-scheme (caddr ,expr) ,env))
         (env ,env))
     ,@body))

(defun +-two (expr env) (+ 
                          (parse-mini-scheme (cadr expr) env)
                          (parse-mini-scheme (caddr expr) env)))

#|
(defun --two (expr env) (- 
                          (parse-mini-scheme (cadr expr) env)
                          (parse-mini-scheme (caddr expr) env)))
|#

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

; heap-record is '(:heap 1 2 3 ... )
(defun heap (expr env)
  (copy-list expr))

(defun record-ref (expr env)
  (let ((heap-record (cadr expr))
        (pos (caddr expr)))
    (incf pos)
    (nth pos heap-record)))

(defun record-set! (expr env)
  (let ((heap-record (cadr expr))
        (pos (caddr expr))
        (value (caddr expr)))
    (incf pos)
    (setf (nth pos heap-record) value)))
