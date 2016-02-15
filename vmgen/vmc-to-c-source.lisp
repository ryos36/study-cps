;----------------------------------------------------------------
(in-package :sinby.cps.vmc-to-c-source)

;(load "package.lisp")
;(load "vmgen.lisp")

;(use-package :cps-vmgen)

;----------------------------------------------------------------
(defclass vmc-to-c-source ()
  ((vmgen :initarg :vmgen :accessor vmgen )))

;----------------------------------------------------------------
(defmacro make-converter (func-name primitive-func-assoc-list)
  (let* ((vmgen-sym (gensym))
         (case-list
           (mapcar #'(lambda (apair)
                       `(,(car apair) (apply ,(cdr apair) `(,,vmgen-sym ,@(cdr vm-code))))) primitive-func-assoc-list )))
    `(defmethod ,func-name ((converter vmc-to-c-source) vm-code)
       (let ((,vmgen-sym (vmgen converter)))
         (print `(:vm-code ,vm-code))
         (if (symbolp vm-code)
           (mark-label ,vmgen-sym vm-code)

           (let ((op (car vm-code))
                 (args (cadr vm-code)))
             (case op
               ,@case-list
               (otherwise 
                 (format *error-output* "~%unknown code:~s~%" op)
                 (sleep 1)))))))))

;----------------------------------------------------------------
(make-converter convert 
                ((:+ . #'primitive-+)
                 (:- . #'primitive--)
                 (:* . #'primitive-*)
                 (:/ . #'primitive-/)

                 (:>> . #'primitive->>)
                 (:<< . #'primitive-<<)

                 (:< . #'primitive-<)
                 (:> . #'primitive->)
                 (:>= . #'primitive->=)
                 (:<= . #'primitive-<=)
                 (:= . #'primitive-eq)
                 (:/= . #'primitive-neq)
                 (:neq . #'primitive-neq)
                 (:<= . #'primitive-<)

                 (:jump . #'primitive-jump)
                 (:conditional-jump . #'primitive-conditional-jump)

                 (:heap . #'primitive-heap)
                 (:stack . #'primitive-stack)
                 (:pop . #'primitive-pop)

                 (:record-ref . #'primitive-record-ref)
                 (:record-offs . #'primitive-record-offs)
                 (:record-set! . #'primitive-record-set!)

                 (:move . #'primitive-move)
                 (:swap . #'primitive-swap)
                 (:movei . #'primitive-movei)
                 (:halt . #'primitive-halt)
                 (:label . #'primitive-label)
                 (:live-reg . #'primitive-live-reg)
                 (:const . #'primitive-const)))

;----------------------------------------------------------------
(defmethod convertx ((converter vmc-to-c-source) vm-code)
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

          (:< (primitive-< vmgen (car args) (cadr args) label-sym))
          (:> (primitive-> vmgen (car args) (cadr args) label-sym))
          (:>= (primitive->= vmgen (car args) (cadr args) label-sym))
          (:<= (primitive-<= vmgen (car args) (cadr args) label-sym))
          (:= (primitive-eq vmgen (car args) (cadr args) label-sym))
          (:/= (primitive-neq vmgen (car args) (cadr args) label-sym))
          (:neq (primitive-neq vmgen (car args) (cadr args) label-sym))
          (:<= (primitive-< vmgen (car args) (cadr args) label-sym))

          (:jump (primitive-jump vmgen (cadr vm-code)))
          (:jump-cond (primitive-cond-jump vmgen (cadr vm-code)))

          (:heap (primitive-heap vmgen (cadr vm-code) (caaddr vm-code)))
          (:stack (primitive-stack vmgen (cadr vm-code) (caaddr vm-code)))
          (:pop (primitive-pop vmgen (cadr vm-code)))

          (:record-ref (primitive-record-ref vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))
          (:record-offs (apply #'primitive-record-offs `(,vmgen ,@(cdr vm-code))))
          (:record-set! (primitive-record-set! vmgen (caadr vm-code) (cadadr vm-code) (caaddr vm-code)))

          (:move (primitive-move vmgen (cadr vm-code) (caddr vm-code)))
          (:swap (primitive-swap vmgen (cadr vm-code) (caddr vm-code)))
          (:movei (primitive-movei vmgen (cadr vm-code) (caddr vm-code)))
          (:halt (primitive-halt vmgen (cadr vm-code)))
          (:label (primitive-label vmgen (cadr vm-code)))
          (:live-reg (primitive-live-reg vmgen (cadr vm-code) (caddr vm-code)))
          (:const (primitive-const vmgen (cadr vm-code)))

          (otherwise 
            (format *error-output* "~%unknown code:~s~%" op)))))))

