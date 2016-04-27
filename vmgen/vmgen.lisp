;----------------------------------------------------------------
(in-package :sinby.cps.vmgen)

;----------------------------------------------------------------
(defclass vmgen ()
  ((registers :accessor registers :initform '
              (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :gp0))
   (code-pos :initform 0 :accessor code-pos)
   (codes :accessor codes :initform nil)
   (types :initform '(:REG :IMM8 :IMM32) :reader types)
   (label-pos-pair :initform nil :accessor label-pos-pair)
   (insn-pos-pair :initform nil :accessor insn-pos-pair)
   (label-offset-pos-pair :initform nil :accessor label-offset-pos-pair)
   (address-pos-pair :initform nil :accessor address-pos-pair)
   (global-value-offset-pos-pair :initform nil :accessor global-value-offset-pos-pair)

   (tag-n :initform 2 :accessor tag-n)
   (integer-tag-value :initform 1 :accessor integer-tag-value)
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
(create-mark-func mark-global-value-offset global-value-offset-pos-pair)

;----------------------------------------------------------------
(defmethod mark-label ((vmgen vmgen) label)
  (push label (codes vmgen))
  (push (cons label (code-pos vmgen)) (label-pos-pair vmgen)))

;----------------------------------------------------------------
(defmethod eval-offset ((vmgen vmgen) sym0 sym1)
  (let* ((label-pos-pair (label-pos-pair vmgen))
         (pos0 (assoc sym0 label-pos-pair))
         (pos1 (assoc sym1 label-pos-pair)))
    (if (and pos0 pos1)
      (- (cdr pos1) (cdr pos0)))))

;----------------------------------------------------------------
(defmethod integer->cell ((vmgen vmgen) value)
  (let ((nagative-bit (if (< value 0) #x80000000 0)) 
        (abs-value (logand (ash value (tag-n vmgen)) #x7FFFFFFF)))
    (assert (= 0 (logand (abs value) #x60000000)))
    (logior nagative-bit abs-value (integer-tag-value vmgen))))

;----------------------------------------------------------------
(defmethod tagged-integer->cell ((vmgen vmgen) tagged-integer)
  (let ((rv
          (if (consp tagged-integer)
            (let ((key (car tagged-integer))
                  (value (cadr tagged-integer)))
              (if (eq key :INTEGER)
                (integer->cell vmgen value))))))
    (if rv rv tagged-integer)))

;----------------------------------------------------------------
(defmethod make-operand ((vmgen vmgen) x0-type x1-type x2-type r0-value r1-value r2-value)
  ;(print `(:make-operand ,r0-value ,r1-value ,r2-value))
  (let ((types (types vmgen)))
    (let ((x0-type-n (position x0-type (types vmgen)))
          (x1-type-n (position x1-type (types vmgen)))
          (x2-type-n (position x2-type (types vmgen)))
          (r0-value8 (logand r0-value #xff))
          (r1-value8 (logand r1-value #xff))
          (r2-value8 (logand r2-value #xff)))
      (assert (and x0-type-n x1-type-n x2-type-n
                   (= r0-value r0-value8)
                   (= r1-value r1-value8)
                   (= r2-value r2-value8)))

      (logior
        (ash x0-type-n (+ 4 24))
        (ash x1-type-n (+ 2 24))
        (ash x2-type-n (+ 0 24))

        (ash r0-value8 16)
        (ash r1-value8  8)
        (ash r2-value8  0)))))

;----------------------------------------------------------------
(defmethod add-code ((vmgen vmgen) code)
  (push (tagged-integer->cell vmgen code) (codes vmgen))

    (if (consp code)
      (let ((key (car code))
            (value (cadr code)))
        (case key
          (:INSTRUCTION (mark-instruction vmgen value))
          (:LABEL (mark-label-offset vmgen value))
          (:ADDRESS (mark-address vmgen value))
          (:OFFSET (mark-global-value-offset vmgen (cdr code)))
          (:INTEGER t)
          (otherwise (assert (eq "not supported list" code))))))

    (incf (code-pos vmgen))

    t)

;----------------------------------------------------------------
(defmethod get-codes ((vmgen vmgen))
  (reverse (codes vmgen)))

;----------------------------------------------------------------
;(defmacro deprecatd-format-incf (&rest body)
;  `(prog1
;     (format ,@body)
;     (incf (code-pos vmgen))))

;----------------------------------------------------------------
(defun symbol-to-c-label (sym)
  (substitute-if #\_  #'(lambda (c) (find c "?*-+:")) (format nil "~a" sym)))

;----------------------------------------------------------------
(defun number-or-bool->number (num-or-sym &optional registers)
  ;(print `(:num-or-sym ,num-or-sym))
  (if (listp num-or-sym) num-or-sym

    (if (numberp num-or-sym) num-or-sym
      (let ((sym num-or-sym))
        (assert (symbolp sym))
        (case sym
          (:|#T| 1) ; integer 0 !! tricky
          (:|#F| 0) ; zero pointer
          (:UNSPECIFIED -1)
          (otherwise 
            (let ((pos (position sym registers)))
              (assert pos)
              pos)))))))

;----------------------------------------------------------------
(defmethod get-value-type ((vmgen vmgen) ax registers)
  (if (numberp ax)
    (if (<= 0 ax 255)
      (values (logand ax #b11111111) :IMM8)
      (values 0 :IMM32))

    (if (listp ax)
      (let ((key (car ax))
            (v (cadr ax))
            (imm32-no-value-for-operand 0))
        (assert (or (eq key :ADDRESS) (eq key :LABEL) (eq key :INTEGER) (eq key :OFFSET)))
        (if (eq key :OFFSET)
          (let ((offset-n (eval-offset vmgen v (caddr ax))))
            (if offset-n
              (if (<= 0 offset-n 255)
                (values offset-n :IMM8)
                (values imm32-no-value-for-operand :IMM32))
              (values imm32-no-value-for-operand :IMM32)))

          (if (and (eq key :INTEGER) (<= 0 v 255))
            (values v :IMM8)
            (values imm32-no-value-for-operand :IMM32))))

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
      (multiple-value-bind (x1 x1-type) (get-value-type vmgen a1 registers)

        ;(print `(,a1 ,x1 ,x1-type))
        (values
          (make-operand vmgen :REG x1-type :REG pos0 x1 pos2)
          x1-type)))))

;----------------------------------------------------------------
(defmacro make-two-args-primitive (func-name code-list)
  `(defmethod ,func-name ((vmgen vmgen) a0 a1 &optional (a2 :r0))
     ;(print `(:args ,a0 ,a1 ,a2))
     (let ((types (types vmgen)))
       (multiple-value-bind (operand x1-type) (reg-pos vmgen a0 a1 a2)
         ,(if (eq (car code-list) :NA) `(assert (not (eq x1-type :REG))))
         ,(if (eq (cadr code-list) :NA) `(assert (not (eq x1-type :IMM8))))
         ,(if (eq (caddr code-list) :NA) `(assert (not (eq x1-type :IMM32))))

         (let ((inst-str
                 (cdr
                   (assoc x1-type `((,(car types)   . ,,(car code-list))
                                    (,(cadr types)  . ,,(cadr code-list))
                                    (,(caddr types) . ,,(caddr code-list)))))))

           (add-code vmgen `(:INSTRUCTION ,inst-str)))

         (add-code vmgen operand)

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
  (let* ((registers (registers vmgen))
         (types (types vmgen))
         (args (cdr tagged-heap-list))
         (len (length args)))
    ;(print `(:phor ,op-str ,args ,a2 ,len))
    (assert (<= len 256))
    (multiple-value-bind (operand x1-type) (reg-pos vmgen :r0 len a2)
      (add-code vmgen `(:INSTRUCTION ,op-str))
      (add-code vmgen operand)
      
      (labels ((format-heap-list (hlist)
                (if (not (null hlist))
                  (let* ((top16 (subseq hlist 0 (min (length hlist) 16)))
                         (v
                           (reduce #'(lambda (i0 i1)
                                       (+ (ash i0 2) i1))
                                   (mapcar #'(lambda (x)
                                               (multiple-value-bind (v type) (get-value-type vmgen x registers)
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
  (assert (or (numberp arg0) (and (consp arg0) (eq (car arg0) :INTEGER))))
  (add-code vmgen (copy-list '(:INSTRUCTION "popi8")))
  (add-code vmgen (cadr arg0))) ; always (:INTEGER <val>)

;----------------------------------------------------------------
(defmethod primitive-record-ref ((vmgen vmgen) a0 a1 a2)
  (if (atom a0)
    (primitive-record-ref-using-reg vmgen a0 a1 a2)

    ;here covers only (:ADDRESS imm32)
    (let ((address-key (car a0))
          (name (cadr a0))
          (registers (registers vmgen)))

      ;(print `(:primitive-record-ref ,a0 ,a1 ,a2))

      (assert (eq address-key :ADDRESS))

      (multiple-value-bind (x1-value x1-type) (get-value-type vmgen a1 registers)
        (assert (eq x1-type :IMM8))
        (let* ((r2 (position a2 registers))

               (operand (make-operand vmgen :IMM32 :IMM8 :REG 0 x1-value r2)))

          (add-code vmgen (copy-list '(:INSTRUCTION "record_refi8_address")))
          (add-code vmgen operand)
          (add-code vmgen a0))))))
;----------------------------------------------------------------
(make-two-args-primitive primitive-record-ref-using-reg ("record_ref" "record_refi8" :NA))
(make-two-args-primitive primitive-record-offs ("record_offs" "record_offsi8" :NA))

;----------------------------------------------------------------
;(record-set! heap-symbol offset value)
;
(defmethod primitive-record-set! ((vmgen vmgen) a0 a1 a2)
  (let ((registers (registers vmgen))
        (types (types vmgen)))

    (let ((pos0 (position a0 registers)))
      (multiple-value-bind (x1 x1-type) (get-value-type vmgen a1 registers)
        (multiple-value-bind (x2 x2-type) (get-value-type vmgen a2 registers)

          ;(print `(:a0 ,a0 :a1 ,x1 ,x1-type ,a1, :a2 ,a2))
          (assert pos0)

          (add-code vmgen (copy-list '(:INSTRUCTION "record_seti8")))
          (add-code vmgen 
                    (make-operand vmgen :REG x1-type x2-type pos0 x1 x2))

          (if (eq x1-type :IMM32)
            (add-code vmgen (tagged-integer->cell vmgen a1)))

          (if (eq x2-type :IMM32)
            (add-code vmgen (tagged-integer->cell vmgen a2))))))))

;----------------------------------------------------------------
; deprecated
;(defmethod primitive-id ((vmgen vmgen) a0 a1)
;  (if (numberp a0)
;    (primitive-movei vmgen a0 a1)
;    (primitive-move vmgen a0 a1)))

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
  (multiple-value-bind (operand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (add-code vmgen operand)))

;----------------------------------------------------------------
(defmethod primitive-swap ((vmgen vmgen) r0 r1)
  (add-code vmgen (copy-list `(:INSTRUCTION "swap")))
  (multiple-value-bind (operand x1-type) (reg-pos vmgen r0 r1)
    (assert (eq x1-type :REG))
    (add-code vmgen operand)))

;----------------------------------------------------------------
(defmethod primitive-set-flag ((vmgen vmgen) bool-symbol r1)
  (assert (or (eq bool-symbol :#t)
              (eq bool-symbol :#f)))

  (add-code vmgen (copy-list '(:INSTRUCTION "set_flagi32")))
  (add-code vmgen (if (eq bool-symbol :#f) 0 1)))

;----------------------------------------------------------------
; imm/(:label v)/(:address v)/(:integer v)
(defmethod primitive-movei ((vmgen vmgen) imm-or-tagged-value r1)
  (if (and (listp imm-or-tagged-value) (not (eq :INTEGER (car imm-or-tagged-value))))
    (let ((tagged-value imm-or-tagged-value)
          (registers (registers vmgen)))

      (add-code vmgen (copy-list '(:INSTRUCTION "loadi32")))
      (add-code vmgen
                (make-operand vmgen :REG :IMM32 :REG 0 0 (position r1 registers)))

      (add-code vmgen (copy-list tagged-value)))

    (let ((imm (number-or-bool->number imm-or-tagged-value)))
      (multiple-value-bind (operand x1-type) (reg-pos vmgen :r0 imm r1)
        (assert (not (eq x1-type :REG)))

        (add-code vmgen `(:INSTRUCTION ,(if (eq x1-type :IMM32) "movei32" "movei8")))
        (add-code vmgen operand)
        (if (eq x1-type :IMM32)
          (add-code vmgen imm))))))

;----------------------------------------------------------------
(defmethod primitive-halt ((vmgen vmgen) r0)
  (add-code vmgen (copy-list '(:INSTRUCTION "halt")))
  (multiple-value-bind (operand x1-type) (reg-pos vmgen r0 :r0)
    (add-code vmgen operand)))

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
; adhoc
(defun calc-op? (op)
  (find op '(:+ :- :* :/ :>> :<< :=)))

;----------------------------------------------------------------
(defmethod calc ((vmgen vmgen) op args)
  (multiple-value-bind (a0 a1 a2) (values-list args)
     (let ((n0 (cadr a0))
           (n1 (cadr a1)))
       (case op
         (:+ (primitive-movei vmgen `(:INTEGER ,(+ n0 n1)) a2))
         (:- (primitive-movei vmgen `(:INTEGER ,(- n0 n1)) a2))
         (:* (primitive-movei vmgen `(:INTEGER ,(* n0 n1)) a2))
         (:/ (primitive-movei vmgen `(:INTEGER ,(floor (/ n0 n1))) a2))
         (:>> (primitive-movei vmgen `(:INTEGER ,(ash n0 (- n1))) a2))
         (:<< (primitive-movei vmgen `(:INTEGER ,(ash n0 n1)) a2))

         (:= (primitive-set-flag vmgen (if (= n0 n1) :#t :#f) a2))

         (otherwise (assert (eq "no operation" op)))))))

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
           
           (if (and (calc-op? op) (<= 2 (length args)) 
                    (consp (car args)) (eq :INTEGER (caar args))
                    (symbolp (cadr args)))
             (let ((tmp (car args)))
               ;swap
               (setf (car args) (cadr args))
               (setf (cadr args) tmp)))

           ;(print `(:calc-op ,op ,(calc-op? op) ,(and (calc-op? op) (consp (car args)) (eq :INTEGER (caar args)) (consp (cadr args)) (eq :INTEGER (caadr args)))))

           (if (and (calc-op? op) 
                    (consp (car args)) (eq :INTEGER (caar args))
                    (consp (cadr args)) (eq :INTEGER (caadr args)))
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

                 (:move . #'primitive-move)
                 (:swap . #'primitive-swap)
                 (:movei . #'primitive-movei)
                 (:halt . #'primitive-halt)
                 (:label . #'primitive-label)
                 (:live-reg . #'primitive-live-reg)
                 (:const . #'primitive-const)))
