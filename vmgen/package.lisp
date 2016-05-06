;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vmgen
  (:use :cl )
  (:nicknames :cps-vmgen)

  (:export 
    :vmgen
    :make-integer->cell

    :my-macro
    :tag-n

    :convert

    :get-codes
    :insn-pos-pair 
    :address-pos-pair
    :label-offset-pos-pair
    :label-pos-pair

    :to-binary-list
    :write-binary-with-open-file 

    :symbol-to-c-label))

