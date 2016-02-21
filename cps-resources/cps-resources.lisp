;----------------------------------------------------------------
(in-package :sinby.cps.resources)

;----------------------------------------------------------------
(defclass cps-resources ()
  ((debug-mode :initform nil :initarg :debug-mode :accessor debug-mode)
   (global-variables :initform nil :initarg :global-variables :reader global-variables)
   (global-functions :initform nil :initarg :global-fuctions :reader global-fuctions)))

;----------------------------------------------------------------
(defmethod set-debug-mode ((cps-resources cps-resources) key)
  (push key (debug-mode cps-resources)))

;----------------------------------------------------------------
(defmethod debug-mode? ((cps-resources cps-resources) key)
  (find key (debug-mode cps-resources)))

;----------------------------------------------------------------
(defun cps-warning (string &optional (str *error-output*) (need-sleep? t))
  (format str string)
  (if need-sleep?
    (sleep 1)))

;----------------------------------------------------------------
(defmethod add-global-variable ((cps-resources cps-resources) gvar &optional (decl nil))
  (let* ((global-variables (global-variables cps-resources))
         (already-gvar-pair (assoc gvar global-variables)))

    (if already-gvar-pair
      (let ((v (cdr already-gvar-pair)))
        (if decl
          (if v
            (cps-warning (format nil "~%Warning!!!!~%define ~a more than 1~%" gvar))
            (setf (cdr already-gvar-pair) decl))))
      (push (cons gvar decl) (slot-value cps-resources 'global-variables)))))

;----------------------------------------------------------------
(defmethod add-global-function ((cps-resources cps-resources) gfunc)
  (let ((global-functions (global-functions cps-resources))

      (push gfunc (slot-value cps-resources 'global-functions)))))

