;----------------------------------------------------------------
(in-package :sinby.cps.resources)

;----------------------------------------------------------------
(defclass cps-resources ()
  ((debug-mode :initform nil :initarg :debug-mode :accessor debug-mode)))

;----------------------------------------------------------------
(defmethod set-debug-mode ((cps-resources cps-resources) key)
  (push key (debug-mode cps-resources)))

;----------------------------------------------------------------
(defmethod debug-mode? ((cps-resources cps-resources) key)
  (find key (debug-mode cps-resources)))
