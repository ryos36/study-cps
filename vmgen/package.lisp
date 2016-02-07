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

    :mark-label
    :write-out-labels
    ))

