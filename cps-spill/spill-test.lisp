(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

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
(setf old-spill-status nil)
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

(setf old-spill-status (copy-tree spill-status))
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

(print `---------------------------------------)
;===============================
(setf rf
      (create-reference-wrapper parser '((:pop-vars (fp0 2) (r0))
                                         (:pop-vars (fp0 3) (r1))
                                         (:pop-vars (fp0 4) (r2))
                                         (:pop-vars (fp0 5) (r3)))))
(setf sf
      (create-stack-wrapper parser old-spill-status spill-status))
(print `(:crwp ,(funcall sf (funcall rf '(:APP EXIT (0))))))
;(print `(:crwp ,(funcall rf '(:APP EXIT (0)))))

(print `(:vars ,(cddr (cadddr spill-status))))
(setf pf
      (create-pop-wrapper parser spill-status))

(print `(:crwp ,(funcall rf (funcall pf '(:APP EXIT (0))))))
