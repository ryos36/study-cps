;----------------------------------------------------------------
(in-package :sinby.cps.vmgen)

(defparameter *code-pos* 0)
(defparameter *code-array-name* "codes")

;----------------------------------------------------------------
(defclass vmgen ()
  ((registers :accessor registers :initform '
              (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))
   (code-pos :initform 0 :accessor code-pos)
   (code-array-name :initarg :code-array-name :initform "codes" :reader code-array-name)
   (types :initform '(:REG :IMM8 :IMM32 :LABEL) :reader types)
   (tagged-labels :initform nil :accessor tagged-labels)))

;----------------------------------------------------------------
(defmacro format-incf (&rest body)
  `(prog1
     (format ,@body)
     (incf (code-pos vmgen))))

;----------------------------------------------------------------
(defun symbol-to-c-label (sym)
  (substitute #\_  #\:
              (format nil "~a" sym)))

;----------------------------------------------------------------
(defun label-to-c-macro-name (vmgen sym-or-c-label)
  (let ((c-label (if (symbolp sym-or-c-label) (symbol-to-c-label sym-or-c-label) sym-or-c-label)))
    ;(assert (stringp c-label))
    (format nil "__~a_~a__" (code-array-name vmgen) c-label)))

;----------------------------------------------------------------
(defun get-value-type (ax registers)
  (if (numberp ax)
    (if (>= 255 ax 0)
      (values (logand ax #b11111111) :IMM8)
      (values 0 :IMM32))
    (values (logand (position ax registers) #xff) :REG)))

;----------------------------------------------------------------
(defmethod reg-pos ((vmgen vmgen) a0 a1 &optional (a2 :r0))
  (let ((registers (registers vmgen)))
    (let ((pos0 (position a0 registers))
          (pos2 (position a2 registers)))

      ;(print `(:02 ,a0 ,a1 ,pos0 a2 ,pos2))
      (multiple-value-bind (x1 x1-type) (get-value-type a1 registers)

        ;(print `(,x1 ,x1-type))
        (let ((x1-type-no (position x1-type (types vmgen))))
          (values
            (logior
              (ash (ash x1-type-no 2) 24)
              (ash (logand pos0 #xff) 16)
              (ash x1 8)
              (ash (logand pos2 #xff)  0))

            x1-type))))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmethod mark-label ((vmgen vmgen) label)
  (let ((label-c (symbol-to-c-label label)))
    (push (cons label-c (code-pos vmgen)) (tagged-labels vmgen))))

;----------------------------------------------------------------
(defmacro make-two-args-primitive (func-name code-list)
  `(defmethod ,func-name ((vmgen vmgen) a0 a1 a2)
     ;(print `(:args ,a0 ,a1 ,a2))
     (let ((types (types vmgen)))
       (multiple-value-bind (oprand x1-type) (reg-pos vmgen a0 a1 a2)
         ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
         ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
         ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))

         (format t "INST_ADDR(~a),~%" 
                 (cdr
                   (assoc x1-type `((,(car types)   . ,,(car code-list))
                                    (,(cadr types)  . ,,(cadr code-list))
                                    (,(caddr types) . ,,(caddr code-list))))))
         (incf (code-pos vmgen))

         (format t "0x~8,'0x,~%" oprand)
         (incf (code-pos vmgen))

         (when (eq x1-type :IMM32)
           (format t "0x~8,'0x,~%" (logand #xffffffff a1))
           (incf (code-pos vmgen)))))))

;----------------------------------------------------------------
(defmacro make-two-args-compare-primitive (func-name code-list)
  `(defmethod ,func-name ((vmgen vmgen) a0 a1 label-or-reg)
     (let ((types (types vmgen))
           (a2 (if (listp label-or-reg) :r0 label-or-reg)))
       (multiple-value-bind (oprand x1-type) (reg-pos vmgen a0 a1 a2)
         ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
         ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
         ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))
         (format t "INST_ADDR(~a),~%" 
                 (cdr
                   (assoc x1-type `((,(car types)   . ,,(car code-list))
                                    (,(cadr types)  . ,,(cadr code-list))
                                    (,(caddr types) . ,,(caddr code-list))))))
         (incf (code-pos vmgen))

         (format t "0x~8,'0x,~%" oprand)
         (incf (code-pos vmgen))

         (when (eq x1-type :IMM32)
           (format t "0x~8,'0x,~%" (logand #xffffffff a1))
           (incf (code-pos vmgen)))

         (if (listp label-or-reg)
           (format t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen (cadr label-or-reg))))
         (incf (code-pos vmgen))))))

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
(make-two-args-compare-primitive primitive-<= ("greater_eq" "greater_eqi" "greater_eqi32"))

(make-two-args-compare-primitive primitive-eq ("eq" "eqi" "eqi32"))
(make-two-args-compare-primitive primitive-neq ("neq" "neqi" "neqi32"))

;----------------------------------------------------------------
(defmethod primitive-heap-or-stack ((vmgen vmgen) op args a2)
  ;(print `(:phor ,op ,args ,a2))
  (let ((len (length args)))
    (assert (<= len 256))
    (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 len a2)
      (format-incf t "INST_ADDR(~a),~%" op)
      (format-incf t "0x~8,'0x,~%" oprand)
      (labels ((format-heap-list (hlist)
                (if (not (null hlist))
                  (let* ((top16 (subseq hlist 0 (min (length hlist) 16)))
                         (sub-len (length top16))
                         (v
                           (reduce #'(lambda (i0 i1)
                                       (+ (* i0 2) i1))
                                   (mapcar #'(lambda (x)
                                               (if (symbolp x) 0 1)) top16)
                                   :initial-value 0))
                         (v0 (ash v (- 16 sub-len))))
                    (format-incf t "0x~8,'0x,~%" v0)
                    (mapc #'(lambda (x)
                              (if (numberp x)
                                (format-incf t "0x~8,'0x,~%" x)
                                (if (symbolp x)
                                  (format-incf t "~a,~%" x)
                                  (format-incf t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen (cadr x)))))) top16))
                  (format-heap-list (nthcdr 16 hlist)))))

        (format-heap-list args)))))

;----------------------------------------------------------------
(defmethod primitive-heap ((vmgen vmgen) args a2)
  (primitive-heap-or-stack vmgen "heap" args a2))

;----------------------------------------------------------------
(defmethod primitive-stack ((vmgen vmgen) args a2)
  (primitive-heap-or-stack vmgen "stack" args a2))

;----------------------------------------------------------------
(defmethod primitive-pop ((vmgen vmgen) arg0)
  (assert (numberp arg0))
  (format-incf t "INST_ADDR(popi8),~%" )
  (format-incf t "0x~8,'0x,~%" arg0))

;----------------------------------------------------------------
(make-two-args-primitive primitive-record-ref ("record_ref" "record_refi8" :NA))
(make-two-args-primitive primitive-record-offs ("record_offs" "record_offsi8" :NA))

;----------------------------------------------------------------
(defmethod primitive-record-set! ((vmgen vmgen) a0 a1 a2)
  (let ((registers (registers vmgen))
        (types (types vmgen)))
    (multiple-value-bind (x0 x0-type) (get-value-type a0 registers)
      (multiple-value-bind (x1 x1-type) (get-value-type a1 registers)
        (assert (not (eq x1-type :IMM32)))
        (let ((x0-type-no (position x0-type types))
              (x1-type-no (position x1-type types))
              (pos2 (position a2 registers)))

          (format-incf t "INST_ADDR(record_set!),~%" )
          (let ((oprand
                  (logior
                    (ash (logior (ash x0-type-no 4)
                                 (ash x1-type-no 2)) 24)
                    (ash x0 16)
                    (ash x1 8)
                    (ash (logand pos2 #xff)  0))))
            (format-incf t "0x~8,'0x,~%" oprand))

         (if (eq x0-type :IMM32)
           (format-incf t "0x~8,'0x,~%" (logand #xffffffff a0))))))))

;----------------------------------------------------------------
(defmethod primitive-jump ((vmgen vmgen) label-or-reg)
  (format-incf t "INST_ADDR(jump),~%" )
  (if (listp label-or-reg)
    (let ((registers (registers vmgen))
          (label-sym (cadr label-or-reg)))

      (format-incf t "0x~8,'0x,~%" 
                   (ash (ash (position :IMM32 (types vmgen)) 4) 24))
      (format-incf t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen label-sym)))

    (let ((reg0 label-or-reg)
          (registers (registers vmgen)))

      (let ((pos (position reg0 registers)))

        (format-incf t "0x~8,'0x,~%" (ash (logand pos #xff) 16))))))

;----------------------------------------------------------------
(defmethod primitive-move ((vmgen vmgen) r0 r1)
  (format-incf t "INST_ADDR(move),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-swap ((vmgen vmgen) r0 r1)
  (format-incf t "INST_ADDR(swap),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-movei ((vmgen vmgen) imm-or-label r1)
  (format-incf t "INST_ADDR(movei),~%" )
  (if (listp imm-or-label)
    (let ((label-sym (cadr imm-or-label))
          (registers (registers vmgen)))
      (format-incf t "0x~8,'0x,~%" 
                   (logior
                     (ash (ash (position :IMM32 (types vmgen)) 4) 24)
                     (ash (logand (position r1 registers) #xff) 0)))
      (format-incf t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen label-sym)))

    (let ((imm imm-or-label))
      (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 imm r1)
        (assert (not (eq x1-type :REG)))
        (format-incf t "0x~8,'0x,~%" oprand)))))

;----------------------------------------------------------------
(defmethod primitive-halt ((vmgen vmgen) r0)
  (format-incf t "INST_ADDR(halt),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 :r0)
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-const ((vmgen vmgen) v)
  (if (numberp v)
    (format-incf t "0x~8,'0x,~%" v)
    (format-incf t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen (cadr v)))))

;----------------------------------------------------------------
(defmethod write-out-labels ((vmgen vmgen) str)
  (let ((tagged-labels (nreverse (tagged-labels vmgen))))
    ;(print `(:tl ,tagged-labels))
    (mapc #'(lambda (label-pos)
              (format-incf t "#define ~a 0x~8,'0x~%"
                           (label-to-c-macro-name vmgen (car label-pos))
                           (cdr label-pos)))
          tagged-labels)))

