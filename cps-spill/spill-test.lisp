(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")

(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

(load "package.lisp")
(load "cps-spill.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :cps-spill)
(use-package :cps-test)

(setf spill (make-instance 'cps-spill:cps-spill))

(setf spill-status (make-new-spill-list))
(setf test-n 0)
(setf parser spill)
(defun test-spill (live-vars)

  (print :========================)
  (print `(:spill-status ,spill-status))
  (print `(:input ,live-vars))
  (setf spill-status (update-next-spill-list parser live-vars spill-status))
  (print `(:result ,(incf test-n) ,spill-status)))

(test-spill
      `(:live-vars :op
                   (:declare :a0 :a1)
                   (:use :r2)
                   (:live :r0 :r1)))

(test-spill
      `(:live-vars :op
                   (:declare :b0 :b1)
                   (:use :r0)
                   (:live :r3 :r1)))

(test-spill
      `(:live-vars :op
                   (:declare :r0)
                   (:use :r4 :r5)
                   (:live )))

(test-spill
      `(:live-vars :op
                   (:declare :r6)
                   (:use :r4 :r5)
                   (:live )))

(test-spill
      `(:live-vars :op
                   (:declare :r9)
                   (:use :r7 :r8)
                   (:live )))

(test-spill
      `(:live-vars :op
                   (:declare :r12)
                   (:use :r10 :r11)
                   (:live )))

(test-spill
      `(:live-vars :op
                   (:declare :r15)
                   (:use :r13 :r14)
                   (:live )))

(test-spill
      `(:live-vars :op
                   (:declare :r18)
                   (:use :r16 :r17)
                   (:live )))
