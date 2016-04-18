;----------------------------------------------------------------
(in-package :sinby.cps.vmgen)

;----------------------------------------------------------------
(defclass vmgen ()
  ((registers :accessor registers :initform '
              (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))
   (code-pos :initform 0 :accessor code-pos)
   (codes :accessor codes :initform nil)
   (types :initform '(:REG :IMM8 :IMM32) :reader types)
   (label-pos-pair :initform nil :accessor label-pos-pair)
   (insn-pos-pair :initform nil :accessor insn-pos-pair)
   (label-offset-pos-pair :initform nil :accessor label-offset-pos-pair)
   (address-pos-pair :initform nil :accessor address-pos-pair)
   ))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defmacro create-mark-func (func-name slot-name &optional (assert-clouse t))
  `(defmethod ,func-name ((vmgen vmgen) value)
     ,assert-clouse

     (let ((,slot-name (,slot-name vmgen))
           (code-pos (code-pos vmgen)))

       (let ((hit-pair 
               (assoc value ,slot-name :test #'equal)))
         (if hit-pair (push code-pos (cdr hit-pair))
           (push (list value code-pos) (,slot-name vmgen)))))))

;----------------------------------------------------------------
(create-mark-func mark-label-offset label-offset-pos-pair)
(create-mark-func mark-address address-pos-pair)
(create-mark-func mark-instruction insn-pos-pair
                  (assert (stringp value)))

;----------------------------------------------------------------
(defmethod mark-label ((vmgen vmgen) label)
  (push label (codes vmgen))
  (push (cons label (code-pos vmgen)) (label-pos-pair vmgen)))

;----------------------------------------------------------------
(defmethod add-code ((vmgen vmgen) code)
  (push code (codes vmgen))

  (if (consp code)
    (let ((key (car code))
          (value (cadr code)))
      (case key
        (:INSTRUCTION (mark-instruction vmgen value))
        (:LABEL (mark-label-offset vmgen value))
        (:ADDRESS (mark-address vmgen value))
        (otherwise (assert (eq "not supported list" code))))))

  (incf (code-pos vmgen))
  code)

;----------------------------------------------------------------
(defmethod get-codes ((vmgen vmgen))
  (reverse (codes vmgen)))

;----------------------------------------------------------------
(defmacro deprecatd-format-incf (&rest body)
  `(prog1
     (format ,@body)
     (incf (code-pos vmgen))))

;----------------------------------------------------------------
(defun symbol-to-c-label (sym)
  (substitute-if #\_  #'(lambda (c) (find c "?*-+:")) (format nil "~a" sym)))

;----------------------------------------------------------------
(defun number-or-bool->number (num-or-sym &optional registers)
  (if (listp num-or-sym) num-or-sym
    (if (numberp num-or-sym) num-or-sym
      (let ((sym num-or-sym))
        (assert (symbolp sym))
        (case sym
          (:|#T| 1)
          (:|#F| 0)
          (:UNSPECIFIED -1)
          (otherwise 
            (let ((pos (position sym registers)))
              (assert pos)
              pos)))))))

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

      ;(print `(:o2 ,a0 ,a1 ,pos0 :a2 ,pos2))
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
(defmacro make-two-args-primitive (func-name code-list)
  `(defmethod ,func-name ((vmgen vmgen) a0 a1 &optional (a2 :r0))
     ;(print `(:args ,a0 ,a1 ,a2))
     (let ((types (types vmgen)))
       (multiple-value-bind (oprand x1-type) (reg-pos vmgen a0 a1 a2)
         ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
         ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
         ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))

         (let ((inst-str
                 (cdr
                   (assoc x1-type `((,(car types)   . ,,(car code-list))
                                    (,(cadr types)  . ,,(cadr code-list))
                                    (,(caddr types) . ,,(caddr code-list)))))))

           (add-code vmgen `(:INSTRUCTION ,inst-str)))

          (add-code vmgen oprand)

          (when (eq x1-type :IMM32)
            (add-code vmgen a1))))))

;----------------------------------------------------------------
(make-two-args-primitive primitive-+ ("add" "addi8" "addi32"))
(make-two-args-primitive primitive-- ("sub" "subi8" "subi32"))
(make-two-args-primitive primitive-* ("mul" "muli8" "muli32"))
(make-two-args-primitive primitive-/ ("div" :NA :NA))

(make-two-args-primitive primitive->> ("shift_r" "shift_ri8" ":NA"))
(make-two-args-primitive primitive-<< ("shift_l" "shift_li8" ":NA"))

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
(defmethod primitive-heap-or-stack ((vmgen vmgen) op-str tagged-heap-list a2)
  ;(print `(:phor ,op-str ,args ,a2))
  (let* ((registers (registers vmgen))
         (types (types vmgen))
         (args (cdr tagged-heap-list))
         (len (length args)))
    (assert (<= len 256))
    (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 len a2)
      (add-code vmgen `(:INSTRUCTION ,op-str))
      (add-code vmgen oprand)
      
      (labels ((format-heap-list (hlist)
                (if (not (null hlist))
                  (let* ((top16 (subseq hlist 0 (min (length hlist) 16)))
                         (v
                           (reduce #'(lambda (i0 i1)
                                       (+ (ash i0 2) i1))
                                   (mapcar #'(lambda (x)
                                               (multiple-value-bind (v type) (get-value-type x registers)
                                                 (if (eq type :REG) 0 2)))
                                           (reverse top16))
                                   :initial-value 0)))

                    (add-code vmgen v)
                    (mapc #'(lambda (x) 
                              (add-code vmgen (number-or-bool->number x registers)))
                          top16))
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
  (add-code vmgen (copy-list '(:INSTRUCTION "popi8")))
  (add-code vmgen arg0))

;----------------------------------------------------------------
(defmethod primitive-record-ref ((vmgen vmgen) a0 a1 a2)
  (if (atom a0)
    (primitive-record-ref-pure vmgen a0 a1 a2)

    (let ((address-key (car a0))
          (name (cadr a0))
          (registers (registers vmgen)))

      (assert (eq address-key :ADDRESS))

      (multiple-value-bind (x1-value x1-type) (get-value-type a1 registers)
        (assert (not (eq x1-type :IMM32)))
        (let* ((pos2 (position a2 registers))
               (types (types vmgen))
               (imm32-no (position :IMM32 types))
               (x1-type-no (position x1-type types))

               (oprand
                 (logior
                   (ash 
                     (logior
                       (ash imm32-no 4)
                       (ash x1-type-no 2)) 24)
                   (ash 0 16)
                   (ash x1-value 8)
                   (ash (logand pos2 #xff)  0))))

          (add-code vmgen (copy-list
                            (if (eq x1-type :IMM8)
                              '(:INSTRUCTION "record_refi8_address")
                              '(:INSTRUCTION "record_ref_address"))))

          (add-code vmgen oprand)
          (add-code vmgen a0))))))
;----------------------------------------------------------------
(make-two-args-primitive primitive-record-ref-pure ("record_ref" "record_refi8" :NA))
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

          (add-code vmgen (copy-list '(:INSTRUCTION "record_set")))
          (let ((oprand
                  (logior
                    (ash (logior (ash x0-type-no 4)
                                 (ash x1-type-no 2)) 24)
                    (ash x0 16)
                    (ash x1 8)
                    (ash (logand pos2 #xff)  0))))
            (add-code vmgen oprand))

         (if (eq x0-type :IMM32)
           (add-code vmgen a0)))))))

;----------------------------------------------------------------
(defmethod primitive-id ((vmgen vmgen) a0 a1)
  (if (numberp a0)
    (primitive-movei vmgen a0 a1)
    (primitive-move vmgen a0 a1)))

;----------------------------------------------------------------
(defmethod primitive-jump-or-conditional-jump ((vmgen vmgen) op-str op-stri32 label-or-reg)
  (if (listp label-or-reg)
    (let ((registers (registers vmgen))
          (label label-or-reg))

      (add-code vmgen `(:INSTRUCTION ,op-stri32))
      (add-code vmgen (ash (ash (position :IMM32 (types vmgen)) 4) 24))
      (add-code vmgen (copy-list label)))

    (let ((reg0 label-or-reg)
          (registers (registers vmgen)))

      (let ((pos (position reg0 registers)))
        (assert (numberp pos))

        (add-code vmgen `(:INSTRUCTION ,op-str))
        (add-code vmgen (ash (logand pos #xff) 16))))))

;----------------------------------------------------------------
(defmethod primitive-jump ((vmgen vmgen) label-or-reg)
  (primitive-jump-or-conditional-jump vmgen "jump" "jumpi32" label-or-reg))

;----------------------------------------------------------------
(defmethod primitive-conditional-jump ((vmgen vmgen) label-or-reg)
  (primitive-jump-or-conditional-jump vmgen "conditional_jump" "conditional_jumpi32" label-or-reg))

;----------------------------------------------------------------
(defmethod primitive-move ((vmgen vmgen) r0 r1)
  (add-code vmgen (copy-list `(:INSTRUCTION "move")))
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (add-code vmgen oprand)))

;----------------------------------------------------------------
(defmethod primitive-swap ((vmgen vmgen) r0 r1)
  (add-code vmgen (copy-list `(:INSTRUCTION "swap")))
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (add-code vmgen oprand)))

;----------------------------------------------------------------
(defmethod primitive-set-flag ((vmgen vmgen) bool-symbol r1)
  (assert (or (eq bool-symbol :#t)
              (eq bool-symbol :#f)))

  (add-code vmgen (copy-list '(:INSTRUCTION "set_flagi32")))
  (add-code vmgen (if (eq bool-symbol :#f) 0 1)))

;----------------------------------------------------------------
(defmethod primitive-movei ((vmgen vmgen) imm-or-label r1)
  (if (listp imm-or-label)
    (let ((label imm-or-label)
          (registers (registers vmgen)))

      (add-code vmgen (copy-list '(:INSTRUCTION "loadi32")))
      (add-code vmgen
        (logior
          (ash (ash (position :IMM32 (types vmgen)) 4) 24)
          (ash (logand (position r1 registers) #xff) 0)))
      (add-code vmgen (copy-list label)))

    (let ((imm (number-or-bool->number imm-or-label)))
      (assert (numberp imm))
      (multiple-value-bind (oprand x1-type) (reg-pos vmgen :r0 imm r1)
        (assert (not (eq x1-type :REG)))

        (add-code vmgen `(:INSTRUCTION ,(if (eq x1-type :IMM32) "movei32" "movei8")))
        (add-code vmgen oprand)
        (if (eq x1-type :IMM32)
          (add-code vmgen imm))))))

;----------------------------------------------------------------
(defmethod primitive-halt ((vmgen vmgen) r0)
  (add-code vmgen (copy-list '(:INSTRUCTION "halt")))
  (multiple-value-bind (oprand x1-type) (reg-pos vmgen r0 :r0)
    (add-code vmgen oprand)))

;----------------------------------------------------------------
(defmethod primitive-const ((vmgen vmgen) v)
  (if (listp v)
    (add-code vmgen (copy-list v))
    (add-code vmgen v)))

;----------------------------------------------------------------
;(defmethod primitive-label ((vmgen vmgen) label-sym)
;  (add-code vmgen (copy-list label-sym)))

;----------------------------------------------------------------
(defmethod primitive-live-reg ((vmgen vmgen) heap-n usage-of-registers)
  (add-code vmgen (copy-list '(:INSTRUCTION "live_reg")))
  (add-code vmgen heap-n)
  (labels ((bit->hex (usage-of-registers0 rv)
            (if (null usage-of-registers0) rv
              (bit->hex (cdr usage-of-registers0) (+ (ash rv 1) (car usage-of-registers0))))))
    (add-code vmgen (bit->hex usage-of-registers 0))))

;----------------------------------------------------------------
(defun calc-op? (op)
  (find op '(:+ :- :* :/ :>> :<< :=)))

;----------------------------------------------------------------
(defmethod calc ((vmgen vmgen) op args)
   (multiple-value-bind (a0 a1 a2) (values-list args)
     (case op
       (:+ (primitive-movei vmgen (+ a0 a1) a2))
       (:- (primitive-movei vmgen (- a0 a1) a2))
       (:* (primitive-movei vmgen (* a0 a1) a2))
       (:/ (primitive-movei vmgen (floor (/ a0 a1)) a2))
       (:>> (primitive-movei vmgen (ash a0 (- a1)) a2))
       (:<< (primitive-movei vmgen (ash a0 a1) a2))

       (:= (primitive-set-flag vmgen (if (= a0 a1) :#t :#f) a2))

       (otherwise (assert (eq "no operation" op))))))

;----------------------------------------------------------------
(defmacro make-converter (func-name primitive-func-assoc-list)
  (let* ((case-list
           (mapcar #'(lambda (apair)
                       `(,(car apair) (apply ,(cdr apair) `(,vmgen ,@(cdr vm-code))))) primitive-func-assoc-list )))
    `(defmethod ,func-name ((vmgen vmgen) vm-code)
       ;(print `(:vm-code ,vm-code))
       (if (symbolp vm-code)
         (mark-label vmgen vm-code)

         (let ((op (car vm-code))
               (args (cdr vm-code))
               (len (length vm-code)))
           
           (if (and (calc-op? op) (<= 2 (length args)) (numberp (car args)) (symbolp (cadr args)))
             (let ((tmp (car args)))
               (print `(:cargs ,args))
               (setf (car args) (cadr args))
               (setf (cadr args) tmp)
               (print `(:args ,args))))

           (if (and (calc-op? op) (numberp (car args)) (numberp (cadr args)))
             (calc vmgen op args)

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

                 (:jump . #'primitive-jump)
                 (:conditional-jump . #'primitive-conditional-jump)

                 (:heap . #'primitive-heap)
                 (:stack . #'primitive-stack)
                 (:pop . #'primitive-pop)

                 (:record-ref . #'primitive-record-ref)
                 (:record-offs . #'primitive-record-offs)
                 (:record-set! . #'primitive-record-set!)

                 (:id . #'primitive-id)

                 (:move . #'primitive-move)
                 (:swap . #'primitive-swap)
                 (:movei . #'primitive-movei)
                 (:halt . #'primitive-halt)
                 (:label . #'primitive-label)
                 (:live-reg . #'primitive-live-reg)
                 (:const . #'primitive-const)))
