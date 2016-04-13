;----------------------------------------------------------------
(in-package :sinby.csf.acps-environment)

;----------------------------------------------------------------
(defclass acps-environment () 
  ((info :initform nil :initarg :info :reader info)))

;----------------------------------------------------------------
(defgeneric add-infomation (env conveter key values))
(defgeneric make-new-environment (env &optional conveter))

;----------------------------------------------------------------
(defmethod add-infomation ((env acps-environment) converter key values)
  (push (list key values) (slot-value env 'info)))

;----------------------------------------------------------------
(defmethod make-new-environment ((upper-env acps-environment) &optional converter)
  (let ((upper-info (copy-tree (slot-value upper-env 'info)))
        (new-env (make-instance 'acps-environment)))
    (add-infomation new-env :upper-info upper-env)))

