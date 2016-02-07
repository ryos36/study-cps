;----------------------------------------------------------------
(in-package :sinby.cps.vmc-to-c-source)

;(load "package.lisp")
;(load "vmgen.lisp")

;(use-package :cps-vmgen)

;----------------------------------------------------------------
(defclass vmc-to-c-source ()
  ((vmgen :initarg :vmgen :accessor vmgen )))

;----------------------------------------------------------------
(defmethod convert ((converter vmc-to-c-source) vm-code)
  (let ((vmgen (vmgen converter)))
    (if (symbolp vm-code)
      (mark-label vmgen vm-code)

      (let ((op (car vm-code))
            (args (cadr vm-code)))

        (case op
          (:+ (primitive-+ vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:- (primitive-- vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:* (primitive-* vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:/ (primitive-/ vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))

          (:>> (primitive->> vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:<< (primitive-<< vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))

          (:jump (primitive-jump vmgen (cadr vm-code)))
          (:jump-cond
            (let ((compare-op (cadr vm-code))
                  (args (caddr vm-code))
                  (label-sym (cadddr vm-code)))
              (case compare-op
                (:< (primitive-< vmgen (car args) (cadr args) label-sym))
                (:> (primitive-> vmgen (car args) (cadr args) label-sym))
                (:>= (primitive->= vmgen (car args) (cadr args) label-sym))
                (:<= (primitive-<= vmgen (car args) (cadr args) label-sym))
                (:= (primitive-eq vmgen (car args) (cadr args) label-sym))
                (:/= (primitive-neq vmgen (car args) (cadr args) label-sym))
                (:neq (primitive-neq vmgen (car args) (cadr args) label-sym))
                (:<= (primitive-< vmgen (car args) (cadr args) label-sym)))))

          (:heap (primitive-heap vmgen (cadr vm-code) (caaddr vm-code)))
          (:stack (primitive-stack vmgen (cadr vm-code) (caaddr vm-code)))
          (:pop (primitive-pop vmgen (cadr vm-code)))

          (:record-ref (primitive-record-ref vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:record-offs (primitive-record-offs vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:record-set! (primitive-record-set! vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))

          (:move (primitive-move vmgen (cadr vm-code) (caddr vm-code)))
          (:swap (primitive-swap vmgen (cadr vm-code) (caddr vm-code)))
          (:movei (primitive-movei vmgen (cadr vm-code) (caddr vm-code)))
          (:halt (primitive-halt vmgen (cadr vm-code)))
          (:const (primitive-const vmgen (cadr vm-code))))))))

