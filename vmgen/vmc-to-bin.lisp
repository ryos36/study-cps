;----------------------------------------------------------------
(in-package :sinby.cps.vmgen)

;----------------------------------------------------------------
(defmethod to-binary-list ((vmgen vmgen))
  (let ((pure-codes (remove-if #'symbolp (get-codes vmgen)))
        (insn-pos-pair (reverse (insn-pos-pair vmgen)))
        (address-pos-pair (reverse (address-pos-pair vmgen)))
        (label-pos-pair (label-pos-pair vmgen))
        (label-offset-pos-pair (reverse (label-offset-pos-pair vmgen))))

    (let ((label-offset-pos-list (sort (reduce #'append (mapcar #'(lambda (x) (cdr x)) (copy-tree label-offset-pos-pair))) #'<))
          (address-pos-list (sort (reduce #'append (mapcar #'(lambda (x) (cdr x)) (copy-tree address-pos-pair))) #'<))
          (insn-pos-list (sort (reduce #'append (mapcar #'(lambda (x) (cdr x)) (copy-tree insn-pos-pair))) #'<)))

      (labels ((round4 (offset size) (* (truncate (+ offset size 3) 4) 4))
               (make-header (magic-no size-list)
                  (multiple-value-bind (header-size codes-size label-offset-pos-size address-pos-size insn-pos-size insn-declare-size) (values-list size-list)
                    (let* ((codes-offset header-size)
                           (label-offset-pos-offset (round4 codes-offset codes-size))
                           (address-pos-offset (round4 label-offset-pos-offset label-offset-pos-size))
                           (insn-pos-offset (round4 address-pos-offset address-pos-size))
                           (insn-declare-offset (round4 insn-pos-offset insn-pos-size)))
                      (list magic-no header-size
                            codes-offset codes-size
                            label-offset-pos-offset label-offset-pos-size
                            address-pos-offset address-pos-size
                            insn-pos-offset insn-pos-size
                            insn-declare-offset insn-declare-size))))

               (quad->uint32 (quad &key (conv #'(lambda(x) x)))
                  (reduce #'(lambda (a b) (+ (ash a 8) (funcall conv b))) quad :initial-value 0))

               (byte->quad-byte (byte-list rv)
                  (if (null byte-list) (nreverse rv)
                    (let* ((top4 (subseq byte-list 0 (min (length byte-list) 4)))
                           (sub-len (length top4)))
                      (push (quad->uint32 (reverse top4)) rv)
                      (byte->quad-byte (nthcdr 4 byte-list) rv))))

               (make-insn-name-byte-list ()
                  (let ((n 0)
                        (rv nil)
                        (len (length insn-pos-pair)))

                    (push (logand len 255) rv)
                    (push (logand (ash len -8) 255) rv)
                    (push 0 rv)
                    (push 0 rv)

                    (dolist (i insn-pos-pair)
                      (push (logand n 255) rv)
                      (push (logand (ash n -8) 255) rv)
                      (incf n)
                      (let* ((insn-str (car i))
                             (clang-len (+ (length insn-str) 1))
                             (clang-len4 (round4 0 clang-len)))
                        (push (logand clang-len 255) rv)
                        (push (logand (ash clang-len -8) 255) rv)
                        (mapc #'(lambda (x) (push x rv))
                                  (map 'list #'char-code insn-str))
                        (push 0 rv)
                        (loop for i from 1 to (- clang-len4 clang-len) do (push 0 rv))
                        ))
                    (nreverse rv)))

               (normalize-codes0 (key x-pos-pair pos-func)
                 (let ((n 0))
                   (mapc #'(lambda (x)
                             (let ((assoc-key (car x))
                                   (pos-list (cdr x)))
                               (dolist (i pos-list)
                                 (let ((replace-n (funcall pos-func assoc-key n))
                                       (elm (elt pure-codes i)))
                                   (assert (and (consp elm)
                                                (eq (car elm) key)
                                                (eq (cadr elm) assoc-key)))
                                   (setf (elt pure-codes i) replace-n)))
                               (incf n))) x-pos-pair)))

               (search-label-pos (key n)
                 (let ((label-pos (assoc key label-pos-pair)))
                   (assert label-pos)
                   (cdr label-pos)))

               (disruptive-simple-flatten (l0 l1 l2 l3 l4)
                  (reduce #'(lambda (ls0 la0)
                              (setf (cdr (last ls0)) la0))
                          (list l0 l1 l2 l3 l4))
                  l0)

               (uint32->byte (ui32 &key (shift-list '(0 -8 -16 -24)))
                 (mapcar #'(lambda (x) (logand (ash ui32 x) #xff))
                             shift-list))

               (uint32->byte-push (ui32 rv &key (shift-list '(-24 -16 -8 0)))
                 (mapc #'(lambda (x) (push (logand (ash ui32 x) #xff) rv))
                             shift-list))

               (normalize-codes ()
                 (normalize-codes0 :INSTRUCTION (copy-list insn-pos-pair) #'(lambda (k n) n))
                 (normalize-codes0 :ADDRESS address-pos-pair #'search-label-pos)
                 (normalize-codes0 :LABEL label-offset-pos-pair #'search-label-pos)
                 pure-codes))

        (normalize-codes)
        (let* ((insn-name-byte-list (make-insn-name-byte-list))
               (insn-name-byte-list-len (round4 0 (length insn-name-byte-list)))
               (inbl-len/4 (/ insn-name-byte-list-len 4))
               (header (make-header
                         (quad->uint32 "VMC" :conv #'char-code)
                         (mapcar #'(lambda (x) (* x 4))
                                 (list
                                   12
                                   (length pure-codes)
                                   (length label-offset-pos-list)
                                   (length address-pos-list)
                                   (length insn-pos-list)
                                   inbl-len/4)))))

          ;(dolist (i header) (format t "~x~%" i))
          ;(print  `(:insn-bytes ,insn-name-byte-list))

          (append
            (reduce #'append
                    (mapcar #'(lambda (x) (uint32->byte x))
                            (disruptive-simple-flatten header pure-codes label-offset-pos-list address-pos-list insn-pos-list)))
            insn-name-byte-list))))))

;----------------------------------------------------------------
(defun write-binary-with-open-file (vmgen bin-file etype)

  (with-open-file (out bin-file :if-does-not-exist :create 
                       :element-type etype
                       :buffered t
                       :direction :output)

    (let ((byte-list (to-binary-list vmgen)))
      ;(print `(:iii ,byte-list))
      (write-sequence byte-list out))))
