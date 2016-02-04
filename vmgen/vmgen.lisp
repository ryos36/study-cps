(defparameter *registers* '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))

(defun reg-pos (a0 a1 a2)
  (let ((pos0 (position a0 *registers*))
        (pos2 (position a2 *registers*)))

    ;(print `(:02 ,a0 ,a1 ,pos0 a2 ,pos2))
    (multiple-value-bind (x1 x1-type)
      (if (numberp a1)
        (if (>= 128 a1 -127)
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

(defun reg-pos-old (pos0 pos1 pos2)
  (logior 
    (ash (logand pos0 #xff) 16)
    (ash (logand pos1 #xff)  8)
    (ash (logand pos2 #xff)  0)))

;----------------------------------------------------------------
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

;----------------------------------------------------------------

(defun primitive-+-old (a0 a1 a2)
  (let ((type-a0 (numberp a0))
        (type-a1 (numberp a1))
        (pos2 (position a2 *registers*)))
    (if (and type-a0 type-a1)
      (let ((n (+ a0 a1)))
        (format t "INST_ADDR(movei),~%")
        (format t "0x~x,~%" n)
        (format t "0x~x,~%"  pos2))
      (if (or type-a1 type-a2)
        (let ((pos0 (position (type-a1 a0 a1) *registers*))
              (n (type-a1 a1 a0)))

          (format t "INST_ADDR(addi),~%")
          (format t "0x~x,~%" (reg-pos pos0 0 pos2))
          (format t "0x~x,~%" n))

        (let ((pos0 (position a0 *registers*))
              (pos1 (position a1 *registers*)))

          (format t "INST_ADDR(add),~%")
          (format t "0x~x,~%" (reg-pos pos0 pos1 pos2)))))))

#|
    (:-  t)
    (:*  t)

    (:>> t)
    (:<< t)

    (:<  t)
    (:>  t)
    (:>= t)
    (:<= t)
    (:=  t)
    (:/=  t)

    (:heap t)
    (:record-set! t)
    (:record-ref t)
    (:record-offs t)
    (:stack t)
    (:pop t)

    (otherwise nil)))
|#
