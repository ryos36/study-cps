(load "package.lisp")
(load "vmgen.lisp")
;(load "vmc-to-c-source.lisp")

(in-package :sinby.cps.vmgen)

(setf vmgen (make-instance 'vmgen))

(defmacro x (a) `(+ 1 ,a))
;(print (macroexpand-1 '(x 3)))
(print (macroexpand-1 '(make-converter convert 
                                       ((:record-offs . #'primitive-record-offs)
                                        (:labell . #'primitive-label)))))

(primitive-+ vmgen :r0 1 :r0)
(primitive-+ vmgen :r0 -1 :r0)
(primitive-+ vmgen :r0 1211 :r0)
(primitive-+ vmgen :r0 :r1 :r0)


(mark-label vmgen '|:label1|)
(primitive-/ vmgen :r0 :r1 :r0)
;(format t "~X~%" (reg-pos :r0 1 :r2))

(primitive-< vmgen :r0 :r1)
(primitive-jump vmgen '(:label |:jump-label1|))
(primitive-conditional-jump vmgen '(:label |:cjump-label1|))

(mark-label vmgen '|:label2|)
(primitive-heap vmgen '(0 1 2 (:label |:label1|) (:address |:main|) :r3 3 4) :r5)
(primitive-record-ref vmgen :r0 0 :r5)
(primitive-record-offs vmgen :r0 1 :r3)

(primitive-record-set! vmgen :r0 1 :r4)
(primitive-record-set! vmgen 400 255 :r6)
(primitive-const vmgen '(:label |:label-label1|))
(primitive-const vmgen '(:address |:address-label1|))
(primitive-const vmgen #x0817)

