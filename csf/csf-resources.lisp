;----------------------------------------------------------------
(in-package :sinby.csf.resources)

;----------------------------------------------------------------
(defgeneric add-global-variable (class gvar &optional decl))

;----------------------------------------------------------------
(defclass csf-resources ()
  ((debug-mode :initform nil :initarg :debug-mode :accessor debug-mode)
   (global-variables :initform nil :initarg :global-variables :reader global-variables)
   (global-functions :initform nil :initarg :global-fuctions :reader global-fuctions)))

;----------------------------------------------------------------
(defmethod set-debug-mode ((csf-resources csf-resources) key)
  (push key (debug-mode csf-resources)))

;----------------------------------------------------------------
(defmethod debug-mode? ((csf-resources csf-resources) key)
  (find key (debug-mode csf-resources)))

;----------------------------------------------------------------
(defun csf-warning (string &optional (str *error-output*) (need-sleep? t))
  (format str string)
  (if need-sleep?
    (sleep 1)))

;----------------------------------------------------------------
(defmethod add-global-variable ((csf-resources csf-resources) gvar &optional (decl nil))
  (let* ((global-variables (global-variables csf-resources))
         (already-gvar-pair (assoc gvar global-variables)))

    (if already-gvar-pair
      (let ((v (cdr already-gvar-pair)))
        (if decl
          (if v
            (csf-warning (format nil "~%Warning!!!!~%define ~a more than 1~%" gvar))
            (setf (cdr already-gvar-pair) decl))))
      (push (cons gvar decl) (slot-value csf-resources 'global-variables)))))

;----------------------------------------------------------------
(defmethod add-global-function ((csf-resources csf-resources) gfunc)
  (let ((global-functions (global-functions csf-resources))

      (push gfunc (slot-value csf-resources 'global-functions)))))

