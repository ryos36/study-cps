(:define usb-host0-address #xE0002000)
(:define /dev/usb-host0 #xE0002000)
;(assign-io /dev/usb-host0 #xE0002000 size)
(:record-ref /dev/usb-host0 0)
(:record-set! /dev/usb-host0 0 value)

null?
car
cdr
eq?
list?

(:define to-num (v-lst) ; ( 3 . 30 )
         (<< (car v-lst) (cdr v-lst)))

(:define bit-op (v32 lst)
  (:if (null? lst)
       v32
       (let ((elm (car lst)))
         (let ((op (car elm))
               (v0 (cdr elm)))
           (let ((v (if (list? v0) (to-num v0) v0)))
             (let ((new-v32
                     (if op
                       (bit-set v32 v)
                       (bit-clear v32 v))))
               (bit-op new-v32 (cdr lst))))))))

(:define set_ulpi_mode ()
         (let ((v32 (:record-ref /dev/usb-host0 0x184)))
           (let ((new-v32 
                  (bit-set
                    (bit-clear
                      (bit-clear v32 (bit-not (<< 3 30)))
                      (bit-not #x1100))
                    (<< 2 30))))

             (:record-set! /dev/usb-host0 0x184 new-v32))))

(:define set_ulpi_mode ()
         (let ((v32 (:record-ref /dev/usb-host0 0x184)))
           (let ((new-v32 
                  (bit-op v32
                          '((#f . (3 . 30))
                            (#f . #x1100)
                            (#t . (2 . 30))))))

             (:record-set! /dev/usb-host0 0x184 new-v32))))

(:define wait-flag (addr offset bit-v value)
         (let ((v (:record-ref addr offset)))
           (let ((flag (:= (:bit-and v bit-v) value)))
             (if flag #t
               (wait-flag addr offset bit-v value)))))

(:define do_soft_reset_for_usb ()
         (let ((v32 (:record-ref /dev/usb-host0 0x184)))
           (let ((new-v32 
                  (bit-set
                    (bit-clear v32 (bit-not (<< 1 30)))
                    (<< 2 30))))

             (:record-set! /dev/usb-host0 0x184 new-v32)
             (wait-flag /dev/usb-host0 #x140 2 0))))


(let ((iQH (calloc align32 sizeof(uint32_t) 7)))
  (reset iQH)
  (set-next iQH iQH 'file)
  (set-xxxx iQH offset value))
            
