(in-package :sinby.cps.resource-scheduler)

;----------------------------------------------------------------
(defclass resource-scheduler () 
  ((name :initarg :name :initform (gensym "scheduler-") :accessor name)
   (resources :initform nil :accessor resources)
   (nodes :initform nil :accessor nodes)
   (initial-nodes :initform nil :accessor initial-nodes)
   (final-nodes :initform nil :accessor final-nodes)
   (ready-nodes :initform nil :accessor ready-nodes)
   (ordered-nodes :initform nil :accessor ordered-nodes)
   (dag-flag :initform nil)))

;----------------------------------------------------------------
; stat -> :init :activate :busy :consumed
(defclass node () 
  ((name :initarg :name :initform (gensym "node-") :accessor name)
   (instruction :initarg :instruction :initform nil :accessor instruction)
   (status :initarg :status :initform :init :accessor status)
   (cost-value :initarg :cost-value :initform 1)
   (input-resources :initarg :input-resources :initform nil :accessor input-resources)
   (output-resources :initarg :output-resources :initform nil :accessor output-resources)
   (special-resources :initarg :speical-resources :initform nil :accessor special-resources)
   (successors :initform nil :accessor successors)))

;----------------------------------------------------------------
; stat -> :init :ready :busy 
(defclass resource () 
  ((name :initarg :name :accessor name)
   (status :initarg :status :initform :init :accessor status)
   (cost-value :initarg :cost-value :initform 1)
   (accounting :initform 1)))

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
(defmethod add-resource ((scheduler resource-scheduler) (res resource))
  (setf (resources scheduler) (append (resources scheduler) (list res))))

;----------------------------------------------------------------
(defmethod add-resource ((scheduler resource-scheduler) sym)
  (let ((res (make-instance 'resource :name sym)))
    (add-resource scheduler res)
    res))

;----------------------------------------------------------------
(defmethod add-resources ((scheduler resource-scheduler) res-syms)
  (mapcar #'(lambda (sym) 
    (let ((has-sym? (get-resource scheduler sym)))
      (if has-sym? has-sym?
        (add-resource scheduler sym)))) (reverse res-syms)))

;----------------------------------------------------------------
(defmethod add-node ((scheduler resource-scheduler) (node node) &optional input-syms output-syms special-syms)
  (push node (nodes scheduler))

  (labels ((to-list (sym) (if (listp sym) sym (list sym))))
    (if input-syms
      (let ((input-res-list (add-resources scheduler (to-list input-syms))))
        (setf (input-resources node) input-res-list)

        (if output-syms
          (let ((output-res-list (add-resources scheduler (to-list output-syms))))
            (setf (output-resources node) output-res-list)

            (if special-syms
              (let ((special-res-list (add-resources scheduler (to-list special-syms))))
                (setf (special-resources node) special-res-list))))))))
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
(defmethod initialize-activate-resources ((scheduler resource-scheduler))
  (let* ((all-resources (resources scheduler))
         (special-or-args-or-free-syms 
           (reduce #'set-difference 
              (mapcar #'(lambda (n) (output-resources n))
                      (nodes scheduler)) :initial-value all-resources)))
    (mapcar #'(lambda (res) (activate-resource scheduler res) res) 
        special-or-args-or-free-syms)))
;----------------------------------------------------------------
(defmethod initialize-ready-nodes ((scheduler resource-scheduler))
  (let ((ready-nodes (copy-tree (initial-nodes scheduler))))
    ;(setf (ready-nodes scheduler) ready-nodes)
    (mapc #'(lambda (node) 
      (set-ready scheduler node)
      (let ((i-res (input-resources node)))
        (mapc #'(lambda (res)
           (activate-resource scheduler res))
              i-res)))
          ready-nodes)))

;----------------------------------------------------------------
(defmethod set-ready ((scheduler resource-scheduler) (node node))
  (let ((ready-nodes (ready-nodes scheduler)))
    (setf (status node) :ready)
    (setf (ready-nodes scheduler) (cons node ready-nodes))))

;----------------------------------------------------------------
(defmethod reset-ready-nodes ((scheduler resource-scheduler))
  (let ((ready-nodes (ready-nodes scheduler)))
    (mapc #'(lambda (node) 
              (if (eq (status node) :ready)
                (setf (status node) :init))) ready-nodes)
    (setf (ready-nodes scheduler) nil)

    ;--
    (mapc #'(lambda (node) 
              (assert (not (eq (status node) :ready))))
          (nodes scheduler))))

;----------------------------------------------------------------
(defmethod update-ready-nodes ((scheduler resource-scheduler))
  (labels ((is-ready? (node)
            (let ((input-resources (input-resources node))
                  (output-resources (output-resources node))
                  (special-resources (special-resources node)))
              (reduce #'(lambda (rv o-res) (and rv (not (eq (status o-res) :busy)))) output-resources :initial-value (reduce #'(lambda (rv res) (and rv (eq (status res) :active))) (append input-resources special-resources) :initial-value t)))))

    (reset-ready-nodes scheduler)

    (let ((all-nodes (nodes scheduler)))
      (mapc #'(lambda (node) 
        (let ((stat (status node)))
          (if (not (eq stat :consumed))  ; :init :busy or :ready
            (if (is-ready? node)
              (setf (status node) :candidate)))))

             all-nodes)


      #|
      ; DEBUG CODE
      (mapc #'(lambda (node) (print `(:node ,(instruction node)
                                            ,(status node)
                  ,(mapcar #'(lambda (i) (status i)) (input-resources node))
                  ,(mapcar #'(lambda (o) (status o)) (output-resources node))
                  ,(mapcar #'(lambda (s) (status s)) (special-resources node)))))
            all-nodes)

      (print `(:check0 ,(check-node-status scheduler)))
      |#

      (mapc #'(lambda (node) 
        (let ((stat (status node)))
          (if (not (eq stat :consumed))  
            (let ((successors (successors node)))
              (mapc #'(lambda (successor-node)
                (if (eq (status successor-node) :candidate) 
                   (setf (status successor-node) :init)))
                    successors )))))

             all-nodes)


      (mapc #'(lambda (node) 
        (let ((stat (status node)))
          (if (eq stat :candidate)
            (set-ready scheduler node))))

            all-nodes))))

;----------------------------------------------------------------
(defmethod check-node-status ((scheduler resource-scheduler))
  (labels ((check-node-status0 (nodes rv)
            (if (null nodes) (reverse rv) ; need copy of tree
              (let* ((node (car nodes))
                     (stat (status node))
                     (statistic-0 (assoc stat rv)))

                (if statistic-0 
                  (setf (cdr statistic-0) (+ (cdr statistic-0) 1))
                  (push `(,stat . 1) rv))

                (check-node-status0 (cdr nodes) rv)))))

    (check-node-status0 (nodes scheduler) '())))

;----------------------------------------------------------------
(defmethod select-candidate-node-to-ready ((scheduler resource-scheduler))
  (labels ((eval-cost (nodes cost resource-n)
             (if (null nodes) (cons cost resource-n)
               (let* ((node (car nodes))
                      (node-cost (get-cost node))
                      (node-res-n (get-resource-size node)))

                 (eval-cost 
                   (append (cdr nodes) (successors node))
                   (+ cost node-cost)
                   (+ resource-n node-res-n))))))

    (let ((candidate-nodes (ready-nodes scheduler)))
      (cond ((null candidate-nodes) nil)
            ((null (cdr candidate-nodes)) (car candidate-nodes)) ; len = 1
            (t 
              (let ((node-cost-list
                 (mapcar #'(lambda (node)
                   (cons node (eval-cost (list node) 0 0))) candidate-nodes)))

                (car
                  (reduce #'(lambda (nc0 nc1)
                    (let ((nc0-cost (cadr nc0))
                          (nc1-cost (cadr nc1)))
                      (cond ((< nc0-cost nc1-cost) nc1)
                            ((> nc0-cost nc1-cost) nc0)
                            (t 
                              (let ((nc0-res-n (cddr nc0))
                                    (nc1-res-n (cddr nc1)))
                                (if (< nc0-res-n nc1-res-n) nc1 nc0))))))
                          node-cost-list))))))))

;----------------------------------------------------------------
(defmethod apply-node ((scheduler resource-scheduler) (node node))
  (let ((cost (get-cost node))
        (all-resouces (get-all-resources node))
        (ready-nodes (ready-nodes scheduler)))

    (setf (ordered-nodes scheduler) (append (ordered-nodes scheduler) (list node)))
    (setf (status node) :consumed)
    (setf (ready-nodes scheduler) (delete node ready-nodes))
    (mapc #'(lambda (res) (set-status res :busy cost)) all-resouces)))

;----------------------------------------------------------------
(defgeneric update-accounting (scheduler &optional n ))

(defmethod update-accounting ((scheduler resource-scheduler)  &optional (n 1))
  (let ((all-resources (resources scheduler)))
    (mapc #'(lambda (res) (update-accounting res n)) all-resources)))

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
(defmethod is-finished? ((scheduler resource-scheduler))
  (null
    (let ((all-nodes (nodes scheduler)))
      (remove-if #'(lambda (stat) (eq stat :consumed))
         (mapcar #'(lambda (node) (status node)) all-nodes)))))

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
(defmethod get-all-resources ((node node))
  (append
    (input-resources node)
    (special-resources node)
    (output-resources node)))

;----------------------------------------------------------------
(defmethod add-successor ((src-node node) (dst-node node))
  (push dst-node (successors src-node)))

;----------------------------------------------------------------
(defmethod print-object ((node node) stream)
  (let* ((insn (instruction node))
         (the-name (if insn insn (name node))))
    (write-string (format nil "#Node [~a ~s ~a ~a]" the-name (status node)
      (mapcar #'(lambda (r) (name r)) (input-resources node))
      (mapcar #'(lambda (r) (name r)) (output-resources node))) stream)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defgeneric set-status (res stat &optional accounting))

(defmethod set-status ((res resource) stat &optional (accounting 0))
  (setf (status res) stat)
  (setf (slot-value res 'accounting) accounting))

;----------------------------------------------------------------
(defmethod update-accounting ((res resource) &optional n)
  (let* ((accounting (slot-value res 'accounting))
         (new-value (- accounting n))
         (stat (status res)))

    (if (and (<= new-value 0) (eq stat :busy))
      (set-status res :active 0)
      (setf (slot-value res 'accounting) (max new-value 0)))))

;----------------------------------------------------------------
(defmethod print-object ((res resource) stream)
  (write-string (format nil "#Resource [~a ~s]" (name res) (status res)) stream))
