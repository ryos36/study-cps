(load "package.lisp")
(load "vmgen.lisp")

(use-package :cps-vmgen)

(setf vmgen (make-instance 'vmgen))

(primitive-+ vmgen :r0 1 :r0)
(primitive-+ vmgen :r0 -1 :r0)
(primitive-+ vmgen :r0 1211 :r0)
(primitive-+ vmgen :r0 :r1 :r0)

(primitive-/ vmgen :r0 :r1 :r0)
;(format t "~X~%" (reg-pos :r0 1 :r2))

(primitive-< vmgen :r0 :r1 '|:label1|)
