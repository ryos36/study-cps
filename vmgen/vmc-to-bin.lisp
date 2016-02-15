;----------------------------------------------------------------
(in-package :sinby.cps.vmc-to-bin)

;(load "package.lisp")
;(load "vmgen.lisp")

;(use-package :cps-vmgen)

;----------------------------------------------------------------
(defclass vmc-to-c-bin (vmgen)
  ((code-pos :initform 0 :accessor code-pos)
   (vmgen :initarg :vmgen :accessor vmgen )))


;----------------------------------------------------------------
(defmethod add-code ((converter vmc-to-c-bin) codes)
  (if (symbolp codes)
    (mark-label codes)
    (incf (code-pos vmgen)))

  (call-next-method))
