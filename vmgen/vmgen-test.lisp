(load "package.lisp")
(load "vmgen.lisp")

(use-package :cps-vmgen)

(setf vmgen (make-instance 'vmgen))

(primitive-+ vmgen :r0 1 :r0)
(primitive-+ vmgen :r0 -1 :r0)
(primitive-+ vmgen :r0 1211 :r0)
(primitive-+ vmgen :r0 :r1 :r0)

(mark-label vmgen '|:label1|)
(primitive-/ vmgen :r0 :r1 :r0)
;(format t "~X~%" (reg-pos :r0 1 :r2))

(primitive-< vmgen :r0 :r1 '|:label1|)

(mark-label vmgen '|:label2|)
(primitive-heap vmgen '(0 1 2 (label |:label1|) :r3 3 4) :r5)
(primitive-record-ref vmgen :r0 0 :r5)
(primitive-record-offs vmgen :r0 1 :r3)

(primitive-record-set! vmgen :r0 1 :r3)
(primitive-record-set! vmgen 400 255 :r3)
(primitive-const vmgen '(label |:label1|))
(primitive-const vmgen #x0817)

(format t "==============~%")
(write-out-labels vmgen *standard-output*)
