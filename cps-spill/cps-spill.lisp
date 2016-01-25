;----------------------------------------------------------------
(in-package :cps-spill)

;----------------------------------------------------------------
(defclass cps-spill (cps-parser)
  (()))

;----------------------------------------------------------------
(def-cps-func cps-bind ((parser cps-spill) expr env)
  (let ((func-name (car expr))
        (args (cadr expr))
        (next-cps (caddr expr)))

    (let* ((new-next-cps (cps-parse parser next-cps env)))

      `(,func-name ,args ,new-next-cps))))

