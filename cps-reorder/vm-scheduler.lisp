;
; Sinby Corp. 2016
;

(in-package :vm-scheduler)

;----------------------------------------------------------------
(defgeneric eval-heap-cost (vm-instruction))

;----------------------------------------------------------------
(defclass vm-scheduler (resource-scheduler)
  ((resources :initform 
        (list 
          (make-instance `resource :name 'mem :status :activate :cost-value 3)
          (make-instance `resource :name 'acc :status :activate)))

   (instruction-info :initform '((:heap . 'mem)
                                 (:recored-ref .'mem)
                                 (:recored-set! . 'mem)
                                 ;(others . 'acc)
                                 ) :accessor instruction-type)

   (costs :initform '((:heap . #'eval-heap-cost)
                      (:recored-ref . 3)
                      (:recored-set! . 2) :accessor costs))))

;----------------------------------------------------------------
(defclass vm-instruction (node)
  (()))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod register-vm-register ((scheduler vm-scheduler) sym)
  (let ((has-sym (get-resource scheduler sym)))
    (if has-sym has-sym
      (make-instance 'resource :name sym))))

;----------------------------------------------------------------
(defmethod register-vm-registers ((scheduler vm-scheduler) reg-sym-list)
  (mapcar #'(lambda (sym)
    (register-vm-register scheduler sym)) reg-sym-list))

;----------------------------------------------------------------
(defmethod get-special-resource ((scheduler vm-scheduler) instruction)
  (let* ((instruction-info (instruction-info vm-scheduler))
         (instruction-type (assoc instruction instruction-info)))
    (get-resource scheduler (if instruction-type (cdr instruction-type) 'acc))))

;----------------------------------------------------------------
(defmethod add-primitive-instruction ((scheduler vm-scheduler) instruction arg-syms results-syms)
  (let* ((cost-value (assoc instruction (costs scheduler)))
         (primtive-instruction 
           (make-instance 'vm-instruction 
             :instruction instruction 
             :cost-value (if cost-value (cdr cost-value) 1)
             :input-resources (register-vm-registers arg-syms)
             :speical-resources `(,(get-special-resource scheduler instruction))
             :output-resources (register-vm-registers results-syms))))

    (add-node scheduler primtive-instruction)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod eval-heap-cost ((instruction vm-instruction))
  (let ((input-resources (input-resources instruction)))

    (+ (length input-resources) 1)))

;----------------------------------------------------------------
(defmethod get-cost ((instruction vm-instruction))
  (labels ((assert-numberp (v) (assert (numberp v)) v))
    (let ((cost-value (slot-value node 'cost-value)))
      (assert (or (numberp cost-value) (functionp cost-value)))

    (if (functionp cost-value)
      (funcall functionp instruction)
      (assert-numberp cost-value)))))
