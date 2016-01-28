;----------------------------------------------------------------
(in-package :vm-codegen)

;----------------------------------------------------------------
(defclass heap-parser (cps-parser)
  ((heap-size :initform 0 :accessor heap-size)
   (stack-size :initform 0 :accessor stack-size)))

;----------------------------------------------------------------
(defmethod reset-size ((parser heap-parser))
  (setf (heap-size parser) 0)
  (setf (stack-size parser) 0))

;----------------------------------------------------------------
(defmethod add-heap-size ((parser heap-parser) size)
  (setf (heap-size parser) (+ (heap-size parser) size)))

;----------------------------------------------------------------
(defmethod add-stack-size ((parser heap-parser) size)
  (setf (stack-size parser) (+ (stack-size parser) size)))

;----------------------------------------------------------------
(def-cps-func cps-primitive ((parser heap-parser) expr env)
  (let ((op (car expr))
        (args (cadr expr))
        (result (caddr expr))
        (next-cpss (cadddr expr)))

    (if (eq op :heap)
      (add-heap-size (length args))
      (if (eq op :stack)
        (add-stack-size (length args))))

    (call-next-method)))

