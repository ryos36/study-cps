(defparameter *registers* '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))
(defparameter *code-pos* 0)
(defparameter *code-array-name* "codes")

;----------------------------------------------------------------
(defun symbol-to-c-label (sym)
  (format nil "~a" sym))

;----------------------------------------------------------------
(defun reg-pos (a0 a1 &optional (a2 :r0))
  (let ((pos0 (position a0 *registers*))
        (pos2 (position a2 *registers*)))

    ;(print `(:02 ,a0 ,a1 ,pos0 a2 ,pos2))
    (multiple-value-bind (x1 x1-type)
      (if (numberp a1)
        (if (>= 255 a1 0)
          (values (logand a1 #b11111111) :IMM8)
          (values 0 :IMM32))
        (values (logand (position a1 *registers*) #xff) :REG))

      ;(print `(,x1 ,x1-type))
      (let ((x1-type-no (position x1-type '(:REG :IMM8 :IMM32 :NOT-DEFINE))))
        (values
          (logior
            (ash (ash x1-type-no 2) 24)
            (ash (logand pos0 #xff) 16)
            (ash x1 8)
            (ash (logand pos2 #xff)  0))

          x1-type)))))

;----------------------------------------------------------------
(defmacro make-two-args-primitive (func-name code-list)
  `(defun ,func-name (a0 a1 a2)
     (multiple-value-bind (oprand x1-type) (reg-pos a0 a1 a2)
       ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
       ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
       ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))
       (format t "INST_ADDR(~a),~%" 
               (cdr
                 (assoc x1-type '((:REG   . ,(car code-list))
                                  (:IMM8  . ,(cadr code-list))
                                  (:IMM32 . ,(caddr code-list))))))
       (incf *code-pos*)

       (format t "0x~8,'0x,~%" oprand)
       (incf *code-pos*)

       (when (eq x1-type :IMM32)
         (format t "0x~8,'0x,~%" (logand #xffffffff a1))
         (incf *code-pos*)))))

;----------------------------------------------------------------
(defmacro make-two-args-compare-primitive (func-name code-list)
  `(defun ,func-name (a0 a1 label)
     (multiple-value-bind (oprand x1-type) (reg-pos a0 a1)
       ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
       ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
       ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))
       (format t "INST_ADDR(~a),~%" 
               (cdr
                 (assoc x1-type '((:REG   . ,(car code-list))
                                  (:IMM8  . ,(cadr code-list))
                                  (:IMM32 . ,(caddr code-list))))))
       (incf *code-pos*)

       (format t "0x~8,'0x,~%" oprand)
       (incf *code-pos*)

       (when (eq x1-type :IMM32)
         (format t "0x~8,'0x,~%" (logand #xffffffff a1))
         (incf *code-pos*))

       (format t "&~a[~a],~%" *code-array-name* (symbol-to-c-label label))
       (incf *code-pos*))))

;----------------------------------------------------------------
(make-two-args-primitive primitive-+ ("add" "addi8" "addi32"))
(make-two-args-primitive primitive-- ("sub" "subi8" "subi32"))
(make-two-args-primitive primitive-* ("mul" "muli8" "muli32"))
(make-two-args-primitive primitive-/ ("div" :NA :NA))

(make-two-args-primitive primitive->> ("shift_r" "shift_ri8" "shift_ri32"))
(make-two-args-primitive primitive-<< ("shift_l" "shift_li8" "shift_li32"))

(make-two-args-primitive primitive-bitand ("and" "andi8" "andi32"))
(make-two-args-primitive primitive-bitor ("or" "ori8" "ori32"))
(make-two-args-primitive primitive-bitxor ("xor" "ori8" "xori32"))

(make-two-args-compare-primitive primitive-> ("less_than" "less_thani" "less_thani32"))
(make-two-args-compare-primitive primitive->= ("less_eq" "less_eqi" "less_eqi32"))

(make-two-args-compare-primitive primitive-< ("greater_than" "greater_thani" "greater_thani32"))
(make-two-args-compare-primitive primitive-< ("greater_eq" "greater_eqi" "greater_eqi32"))

(make-two-args-compare-primitive primitive-eq ("eq" "eqi" "eqi32"))
(make-two-args-compare-primitive primitive-neq ("neq" "neqi" "neqi32"))

;----------------------------------------------------------------
#|
stack pop
           (:heap . ,#'heap-transfer)
           (:record-set! . ,#'record-set!-transfer)
           (:record-ref . ,#'record-ref-transfer)
           (:record-offs . ,#'record-offs-transfer)))
|#

;----------------------------------------------------------------
#|
(defun primitive-+ (a0 a1 a2)
  (multiple-value-bind (oprand x1-type) (reg-pos a0 a1 a2)
    (format t "INST_ADDR(~a),~%" 
            (cdr
              (assoc x1-type '((:REG   . "add")
                               (:IMM8  . "addi8")
                               (:IMM32 . "addi32")))))
    (format t "0x~8,'0x,~%" oprand)
    (if (eq x1-type :IMM32)
      (format t "0x~8,'0x,~%" a1))))
|#

;----------------------------------------------------------------

