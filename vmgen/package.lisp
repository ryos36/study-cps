;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vmgen
  (:use :cl )
  (:nicknames :cps-vmgen)

  (:export 
    :vmgen

    :primitive-+
    :primitive--
    :primitive-*
    :primitive-/

    :primitive->>
    :primitive-<<

    :primitive-bitand
    :primitive-bitor
    :primitive-bitxor

    :primitive->
    :primitive->=

    :primitive-<
    :primitive-<=

    :primitive-eq
    :primitive-neq

    :primitive-heap
    :primitive-stack
    :primitive-pop

    :primitive-record-ref
    :primitive-record-offs
    :primitive-record-set!

    :primitive-const
    :primitive-jump
    :primitive-conditional-jump

    :primitive-halt

    :primitive-move 
    :primitive-swap 
    :primitive-movei

    :primitive-label
    :primitive-live-reg

    :mark-label

    :get-codes
    ))

;----------------------------------------------------------------
(defpackage :sinby.cps.vmc-to-bin
  (:use :cl :sinby.cps.vmgen)
  (:nicknames :cps-vmc-to-bin)
  (:export 

    :vmc-to-c-bin))

;----------------------------------------------------------------
(defpackage :sinby.cps.vmc-to-c-source
  (:use :cl :sinby.cps.vmgen)
  (:nicknames :cps-vmc-to-c-source)
  (:export 

    :vmc-to-c-source

    :make-converter

    :convert))
