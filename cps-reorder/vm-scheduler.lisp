;
; Sinby Corp. 2016
;

(in-package :sinby.cps.vm-scheduler)

;----------------------------------------------------------------
(defgeneric eval-alloc-cost (vm-scheduler instruction args results-syms))

;----------------------------------------------------------------
(defclass vm-scheduler (resource-scheduler)
  ((resources :initform 
        (list 
          (make-instance `resource :name :mem :status :activate :cost-value 3)
          (make-instance `resource :name :acc :status :activate)
          (make-instance `resource :name :jump :status :activate)))

   (instruction-info :initform '((:heap . :mem)
                                 (:record-ref . :mem)
                                 (:record-set! . :mem)
                                 (:stack . :mem)
                                 (:app . :jump)
                                 ;(others . :acc)
                                 ) :accessor instruction-info)

   (costs :initform '((:heap . :eval-alloc-cost)
                      (:stack . :eval-alloc-cost)
                      (:record-ref . 3)
                      (:record-set! . 2)) :accessor costs)))

;----------------------------------------------------------------
(defclass vm-instruction (node)
  ((cps-expr :initarg :cps-expr :accessor cps-expr)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod add-vm-register ((scheduler vm-scheduler) sym)
  (let ((has-sym (get-resource scheduler sym)))
    (if has-sym has-sym
      (add-resource scheduler sym))))

;----------------------------------------------------------------
(defmethod add-vm-registers ((scheduler vm-scheduler) reg-sym-list)
  (mapcar #'(lambda (sym)
    (add-vm-register scheduler sym)) reg-sym-list))

;----------------------------------------------------------------
(defmethod get-special-resource ((scheduler vm-scheduler) instruction)
  (let* ((instruction-info (instruction-info scheduler))
         (instruction-type (assoc instruction instruction-info))
         (res-sym (if instruction-type (cdr instruction-type) :acc)))
    (assert (get-resource scheduler res-sym))
    (get-resource scheduler res-sym)))

;----------------------------------------------------------------
(defmethod add-primitive-instruction ((scheduler vm-scheduler) cps-expr instruction args results-syms)
  ; ignore (:label |x|) and number ex. 1
  (let* ((arg-syms (remove-if #'(lambda (x) (not (cps-symbolp x))) args))
         (cost-value-or-func (cdr (assoc instruction (costs scheduler))))
         ;(x (print `(:cost-value-or-func ,cost-value-or-func ,(functionp cost-value-or-func))))
         (cost-value (if cost-value-or-func 
                       (if (symbolp cost-value-or-func)
                         (eval-alloc-cost scheduler instruction args results-syms)
                         cost-value-or-func)
                       1))

         ;(y (print `(:cost-value ,cost-value)))
         (primtive-instruction 
           (make-instance 'vm-instruction 
             :instruction instruction 
             :cost-value cost-value
             :input-resources (add-vm-registers scheduler arg-syms)
             :speical-resources `(,(get-special-resource scheduler instruction))
             :output-resources (add-vm-registers scheduler results-syms)

             :cps-expr cps-expr)))

    (add-node scheduler primtive-instruction)))

;----------------------------------------------------------------
(defmethod add-apply-instruction ((scheduler vm-scheduler) cps-expr func-name args)
  (let* ((arg-syms (remove-if #'(lambda (x) (not (cps-symbolp x))) args))
         (apply-instruction 
           (make-instance 'vm-instruction 
             :instruction :app 
             :input-resources (add-vm-registers scheduler `(,func-name ,@arg-syms))
             :speical-resources `(,(get-resource scheduler :jump))

             :cps-expr cps-expr)))

    (add-node scheduler apply-instruction)))

;----------------------------------------------------------------
(defmethod eval-alloc-cost ((scheduler vm-scheduler) instruction args results-syms)
    (+ (length args) 2))

