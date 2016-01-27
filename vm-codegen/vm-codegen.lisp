;----------------------------------------------------------------
(in-package :vm-codegen)

;----------------------------------------------------------------
(defclass vm-codegen (cps-parser)
  ((max-n :initarg :max-n :initform 5 :reader max-n)))

;----------------------------------------------------------------
(def-cps-func cps-fix ((parser cps-parser) expr env)
  (let ((fix-op (car expr))
        (binds (cadr expr))
        (next-cps (caddr expr))

        (live-vars-tag-list (caar env))
        (codegen-tag-list (cadar env)))

    (print `(:cps-fix ,codegen-tag-list))
    (assert nil)
        
    (let ((new-binds (cps-binds parser binds env))
          (new-next-cps (cps-parse parser next-cps env)))

      `(,fix-op ,new-binds ,new-next-cps))))
