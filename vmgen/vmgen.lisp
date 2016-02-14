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
   (types :initform '(:REG :IMM8 :IMM32) :reader types)
   (tagged-labels :initform nil :accessor tagged-labels)))

; :ADDRESS is only used for heap/stack. please refer hvm.vmg

;----------------------------------------------------------------
(defmacro format-incf (&rest body)
  `(prog1
     (format ,@body)
     (incf (code-pos vmgen))))

;----------------------------------------------------------------
(defun symbol-to-c-label (sym)
  (substitute-if #\_  #'(lambda (c) (find c "?*-+:")) (format nil "~a" sym)))

;----------------------------------------------------------------
(defun number-or-bool->number (num-or-sym)
  (if (numberp num-or-sym) num-or-sym
    (let ((sym num-or-sym))
      (assert (symbolp sym))
      (case sym
        (:|#T| 1)
        (:|#F| 0)
        (otherwise (assert (eq sym "must be true or false")))))))

;----------------------------------------------------------------
(defmethod convert-arg-to-string ((vmgen vmgen) arg &optional (str t))
  (if (numberp arg)
    (format-incf str "0x~8,'0x,~%" arg)

    (if (listp arg)
      (let ((tag-name (car arg)))
        ;(print `(:tag-name ,tag-name, (eq tag-name :label)))
        (case tag-name
          (:label
            (format-incf str "~a,~%"  (label-to-c-macro-name vmgen (cadr arg))))
          (:address
            (format-incf str "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen (cadr arg))))
          
          (otherwise
            (assert (eq "not support list" arg)))))

      (let ((sym arg))
        (assert (symbolp sym))
        (format-incf str "0x~8,'0x,~%" 
          (case sym
            (:|#T| 1)
            (:|#F| 0)
            (otherwise
              (let ((pos (position sym (registers vmgen))))
                (assert pos)
                pos))))))))

;----------------------------------------------------------------
(defun label-to-c-macro-name (vmgen sym-or-c-label)
  (let ((c-label (if (symbolp sym-or-c-label) (symbol-to-c-label sym-or-c-label) sym-or-c-label)))
    ;(assert (stringp c-label))
    (format nil "__~a_~a__" (code-array-name vmgen) c-label)))

;----------------------------------------------------------------
(defun get-value-type (ax registers)
  (if (numberp ax)
    (if (<= 0 ax 255)
      (values (logand ax #b11111111) :IMM8)
      (values 0 :IMM32))

    (if (listp ax)
      (let ((key (car ax)))
        (assert (or (eq key :ADDRESS) (eq key :LABEL)))
        (values ax :IMM32))

      (case ax
        (:|#T| (values 1 :IMM8))
        (:|#F| (values 0 :IMM8))
        (otherwise
          (values (logand (position ax registers) #xff) :REG))))))

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
  `(defmethod ,func-name ((vmgen vmgen) a0 a1 &optional (a2 :r0))
     ;(print `(:args ,a0 ,a1 ,a2))
     (let ((types (types vmgen)))
       (multiple-value-bind (oprand x1-type) (reg-pos vmgen a0 a1 a2)
         ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
         ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
         ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))

         (format-incf t "INSTRUCTION(~a),~%" 
                     (cdr
                       (assoc x1-type `((,(car types)   . ,,(car code-list))
                                        (,(cadr types)  . ,,(cadr code-list))
                                        (,(caddr types) . ,,(caddr code-list))))))

         (format-incf t "0x~8,'0x,~%" oprand)

         (when (eq x1-type :IMM32)
           (format-incf t "0x~8,'0x,~%" (logand #xffffffff a1)))))))

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

(make-two-args-primitive primitive-> ("less_than" "less_thani8" "less_thani32"))
(make-two-args-primitive primitive->= ("less_eq" "less_eqi8" "less_eqi32"))

(make-two-args-primitive primitive-< ("greater_than" "greater_thani8" "greater_thani32"))
(make-two-args-primitive primitive-<= ("greater_eq" "greater_eqi8" "greater_eqi32"))

(make-two-args-primitive primitive-eq ("eq" "eqi8" "eqi32"))
(make-two-args-primitive primitive-neq ("neq" "neqi8" "neqi32"))

;----------------------------------------------------------------
(defmethod primitive-heap-or-stack ((vmgen vmgen) op tagged-heap-list a2)
  ;(print `(:phor ,op ,args ,a2))
  (let* ((registers (registers vmgen))
         (types (types vmgen))
         (args (cdr tagged-heap-list))
         (len (length args)))
    (assert (<= len 256))
    (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 len a2)
      (format-incf t "INSTRUCTION(~a),~%" op)
      (format-incf t "0x~8,'0x,~%" oprand)
      (labels ((format-heap-list (hlist)
                (if (not (null hlist))
                  (let* ((top16 (subseq hlist 0 (min (length hlist) 16)))
                         (sub-len (length top16))
                         (v
                           (reduce #'(lambda (i0 i1)
                                       (+ (ash i0 2) i1))
                                   (mapcar #'(lambda (x)
                                               (multiple-value-bind (v type) (get-value-type x registers)
                                                 (if (eq type :REG) 0 2)))
                                           top16)
                                   :initial-value 0)))

                    (format-incf t "0x~8,'0x,~%" v)
                    (mapc #'(lambda (x) (convert-arg-to-string vmgen x)) top16))
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
  (format-incf t "INSTRUCTION(popi8),~%" )
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

          (format-incf t "INSTRUCTION(record_set),~%" )
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
(defmethod primitive-jump-or-conditional-jump ((vmgen vmgen) op label-or-reg)
  (if (listp label-or-reg)
    (let ((registers (registers vmgen))
          (label-sym (cadr label-or-reg)))

      (format-incf t "INSTRUCTION(~ai32),~%" op)
      (format-incf t "0x~8,'0x,~%" 
                   (ash (ash (position :IMM32 (types vmgen)) 4) 24))
      (format-incf t "~a,~%" (label-to-c-macro-name vmgen label-sym)))

    (let ((reg0 label-or-reg)
          (registers (registers vmgen)))

      (let ((pos (position reg0 registers)))

        (format-incf t "INSTRUCTION(~a),~%" op)
        (format-incf t "0x~8,'0x,~%" (ash (logand pos #xff) 16))))))

;----------------------------------------------------------------
(defmethod primitive-jump ((vmgen vmgen) label-or-reg)
  (primitive-jump-or-conditional-jump vmgen "jump" label-or-reg))

;----------------------------------------------------------------
(defmethod primitive-conditional-jump ((vmgen vmgen) label-or-reg)
  (primitive-jump-or-conditional-jump vmgen "conditional_jump" label-or-reg))

;----------------------------------------------------------------
(defmethod primitive-move ((vmgen vmgen) r0 r1)
  (format-incf t "INSTRUCTION(move),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-swap ((vmgen vmgen) r0 r1)
  (format-incf t "INSTRUCTION(swap),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-movei ((vmgen vmgen) imm-or-label r1)
  (if (listp imm-or-label)
    (let ((label-sym (cadr imm-or-label))
          (registers (registers vmgen)))

      (format-incf t "INSTRUCTION(movei32),~%" )
      (format-incf t "0x~8,'0x,~%" 
                   (logior
                     (ash (ash (position :IMM32 (types vmgen)) 4) 24)
                     (ash (logand (position r1 registers) #xff) 0)))
      (convert-arg-to-string vmgen imm-or-label))

    (let ((imm (number-or-bool->number imm-or-label)))
      (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 imm r1)
        (assert (not (eq x1-type :REG)))
        (format-incf t "INSTRUCTION(movei~a),~%" (if (eq x1-type :IMM32) 32 8))
        (format-incf t "0x~8,'0x,~%" oprand)
        (if (eq x1-type :IMM32)
          (format-incf t "0x~8,'0x,~%" imm))))))

;----------------------------------------------------------------
(defmethod primitive-halt ((vmgen vmgen) r0)
  (format-incf t "INSTRUCTION(halt),~%" )
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 :r0)
    (format-incf t "0x~8,'0x,~%" oprand)))

;----------------------------------------------------------------
(defmethod primitive-const ((vmgen vmgen) v)
  (if (numberp v)
    (format-incf t "0x~8,'0x,~%" v)
    (if (eq (car v) :address)
      (format-incf t "&~a[~a],~%" (code-array-name vmgen) (label-to-c-macro-name vmgen (cadr v)))
      (format-incf t "~a,~%"  (label-to-c-macro-name vmgen (cadr v))))))

;----------------------------------------------------------------
(defmethod primitive-label ((vmgen vmgen) label-sym)
  (format-incf t "~a,~%" (label-to-c-macro-name vmgen label-sym)))

;----------------------------------------------------------------
(defmethod primitive-live-reg ((vmgen vmgen) heap-n usage-of-registers)
  (format-incf t "INSTRUCTION(live_reg),~%" )
  (format-incf t "0x~8,'0x,~%" heap-n)
  (labels ((bit->hex (usage-of-registers0 rv)
            (if (null usage-of-registers0) rv
              (bit->hex (cdr usage-of-registers0) (+ (ash rv 1) (car usage-of-registers0))))))
    (format-incf t "0x~8,'0x,~%" (bit->hex usage-of-registers 0))))

;----------------------------------------------------------------
(defmethod write-out-labels ((vmgen vmgen) str)
  (let ((tagged-labels (nreverse (tagged-labels vmgen))))
    ;(print `(:tl ,tagged-labels))
    (mapc #'(lambda (label-pos)
              (format-incf t "#define ~a 0x~8,'0x~%"
                           (label-to-c-macro-name vmgen (car label-pos))
                           (cdr label-pos)))
          tagged-labels)))

