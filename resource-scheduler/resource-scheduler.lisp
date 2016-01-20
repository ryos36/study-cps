;----------------------------------------------------------------
(defclass resource-scheduler () 
  ((name :initarg :name :initform (gensym "scheduler-") :accessor name)
   (resources :initform nil :accessor resources)
   (nodes :initform nil :accessor nodes)
   (initial-nodes :initform nil :accessor initial-nodes)
   (final-nodes :initform nil :accessor final-nodes)
   (runnable-nodes :initform nil :accessor runnable-nodes)
   (dag-flag :initform nil)))

;----------------------------------------------------------------
(defclass node () 
  ((name :initarg :name :initform (gensym "node-") :accessor name)
   (status :initarg :status :initform :init :accessor status)
   (cost-value :initarg :cost-value :initform 1)
   (input-resources :initarg :input-resources :initform nil :accessor input-resources)
   (output-resources :initarg :output-resources :initform nil :accessor output-resources)
   (special-resources :initarg :speical-resources :initform nil :accessor special-resources)
   (successors :initform nil :accessor successors)))

;----------------------------------------------------------------
(defclass resource () 
  ((name :initarg :name :accessor name)
   (status :initarg :status :initform :init :accessor status)
   (cost-value :initarg :cost-value :initform 1)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod get-resource ((scheduler resource-scheduler) res-sym)
  (labels ((get-resource0 (resources)
             (if (null resources) nil
               (let ((res (car resources)))
                 (if (eq (name res) res-sym)
                   res
                   (get-resource0 (cdr resources)))))))
    (get-resource0 (resources scheduler))))

;----------------------------------------------------------------
(defmethod register-resource ((scheduler resource-scheduler) (res resource))
  (push res (resources scheduler)))

(defmethod register-resource ((scheduler resource-scheduler) res)
  (register-resource scheduler
                     (make-instance 'resource :name res)))

;----------------------------------------------------------------
(defmethod register-resources ((scheduler resource-scheduler) res-lst)
    (mapc #'(lambda (res) (register-resource scheduler res)) res-lst))

;----------------------------------------------------------------
(defmethod add-node ((scheduler resource-scheduler) (node node) &optional input-resources special-resources output-resources)
    (push node (nodes scheduler))
    (if input-resources
      (let ((i-res (mapcar #'(lambda (r) (get-resource scheduler r)) (if (listp input-resources) input-resources (list input-resources)))))
        (setf (input-resources node) i-res)

        (if special-resources
          (let ((s-res (mapcar #'(lambda (r) (get-resource scheduler r)) (if (listp special-resources) special-resources (list special-resources)))))
            (setf (special-resources node) s-res)

            (if output-resources
              (let ((o-res (mapcar #'(lambda (r) (get-resource scheduler r)) (if (listp output-resources) output-resources (list output-resources)))))
                (setf (output-resources node) o-res)))))))
    (update-cost node)
    node)

;----------------------------------------------------------------
(defmethod build-connection ((scheduler resource-scheduler))
  (let (intermedium-nodes
        (all-nodes (nodes scheduler)))
    (mapc #'(lambda (src-node)
      (let ((o-res (output-resources src-node)))
        (mapc #'(lambda (dst-node)
          (let ((i-res (input-resources dst-node)))
            (if (intersection o-res i-res)
              (progn
                (push dst-node intermedium-nodes)
                (push dst-node (successors src-node))))))

              all-nodes)))
          all-nodes)

    (setf (initial-nodes scheduler) (set-difference all-nodes intermedium-nodes))
    (setf (final-nodes scheduler) 
      (remove-if #'null (mapcar #'(lambda (n)
                                    (if (null (successors n)) n)) all-nodes)))
    ))

;----------------------------------------------------------------
(defmethod is-dag? ((scheduler resource-scheduler))
  (labels ((check-dag () 
             (let* ((all-nodes (nodes scheduler))
                    (nodes (mapcar #'(lambda (n) `(,n . nil)) all-nodes)))
               (labels ((check-dag0 (remain-nodes)
                            (if (null remain-nodes) :DAG
                              (let* ((node (car remain-nodes))
                                     (hit (assoc node nodes)))
                                (if (cdr hit)
                                  :NOT-DAG
                                  (progn
                                    (setf (cdr hit) t)
                                    (check-dag0 (cdr remain-nodes))))))))
                 (check-dag0 all-nodes)))))

  (let ((flag (slot-value scheduler 'dag-flag)))
    (let ((flag (if (null flag) (check-dag) flag))) ; tricky!!
      (setf (slot-value scheduler 'dag-flag) flag)
      (eq flag :DAG)))))

;----------------------------------------------------------------
(defmethod initialize-runnable-nodes ((scheduler resource-scheduler))
  (let ((runnable-nodes (copy-tree (initial-nodes scheduler))))
    (setf (runnable-nodes scheduler) runnable-nodes)
    (mapc #'(lambda (node) 
      (let ((i-res (input-resources node)))
        (mapc #'(lambda (res)
           (activate-resource scheduler res))
              i-res)))
          runnable-nodes)))

;----------------------------------------------------------------
(defmethod update-runnable-nodes ((scheduler resource-scheduler))
  (let ((all-nodes (nodes scheduler)))
    (mapc #'(lambda (node) 
      (if (eq (status node) :init)
        (let* ((all-input-resources (input-resources node))
               (is-runnable?
                 (reduce #'(lambda (rv res) (and rv (eq (status res) :active))) all-input-resources :initial-value t)))
          (if is-runnable?
            (setf (status node) :runnable)))))

          all-nodes)))

;----------------------------------------------------------------
(defmethod select-candidate-node-to-run ((scheduler resource-scheduler))
  (labels ((eval-cost (nodes cost resource-n)
             (if (null nodes) (cons cost resource-n)
               (let* ((node (car nodes))
                      (node-cost (get-cost node))
                      (resource-n (get-resource-size node)))
                 (eval-cost 
                   (append (cdr nodes) (successors nodes))
                   (+ cost node-cost)
                   (+ resource-n node-cost))))))

    (let ((candidate-nodes (runnable-nodes scheduler)))
      (cond ((null candidate-nodes) nil)
            ((null (cdr candidate-nodes)) (car candidate-nodes)) ; len = 1
            (t 
              (let ((node-cost-list
                 (mapcar #'(lambda (node)
                   (cons node (eval-cost (list node)))) candidate-nodes)))

                (car
                  (reduce #'(lambda (nc0 nc1)
                    (let ((nc0-cost (cadr nc0))
                          (nc1-cost (cadr nc1)))
                      (cond ((< nc0-cost nc1-cost) nc1)
                            ((> nc0-cost nc1-cost) nc0)
                            (t 
                              (let ((nc0-res-n (cddr nc0))
                                    (nc1-res-n (cddr nc1)))
                                (if (< nc0-res-n nc1-res-n) nc1 nc0))))))))))))))

;----------------------------------------------------------------
(defgeneric activate-resource (scheduler res))

(defmethod activate-resource ((scheduler resource-scheduler) (res resource))
  (assert (member res (resources scheduler)))
  (setf (status res) :active))

(defmethod activate-resource ((scheduler resource-scheduler) res)
  (let ((r (get-resource scheduler res)))
    (assert r)
    (activate-resource scheduler r)))

;----------------------------------------------------------------
(defmethod print-object ((scheduler resource-scheduler) stream)
  (write-string (format nil "#Scheduler [~a ~a ~a]" 
    (name scheduler)
    (mapcar #'(lambda (r) (name r)) (reverse (resources scheduler)))
    (mapcar #'(lambda (n) (name n)) (reverse (nodes scheduler))))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defgeneric get-cost (node))

(defmethod get-cost ((node node))
  (slot-value node 'cost-value))

;----------------------------------------------------------------
(defmethod update-cost ((node node))
  (setf
    (slot-value node 'cost-value)
    (max 1 (reduce #'+ (mapcar #'(lambda(n) (slot-value n 'cost-value))
                   (special-resources node))))))

;----------------------------------------------------------------
(defgeneric get-resource-size (node))

(defmethod get-resource-size ((node node))
  (+ (length (input-resources node))
     (length (special-resources node))
     (length (output-resources node))))

;----------------------------------------------------------------
(defmethod print-object ((node node) stream)
  (write-string (format nil "#Node [~a ~s ~a ~a]" (name node) (status node)
    (mapcar #'(lambda (r) (name r)) (input-resources node))
    (mapcar #'(lambda (r) (name r)) (output-resources node))) stream))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod print-object ((res resource) stream)
  (write-string (format nil "#Resource [~a ~s]" (name res) (status res)) stream))
