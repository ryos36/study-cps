;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.transfer
  (:use :cl )
  (:nicknames :cps-transfer)

  (:export 
    :do-lisp-to-cps
    :make-transfer-table
    :make-exit-continuous
    :cps-gensym

    :*transfer-table*
    :*debug-mode*
    :*context*
    :*warning*))

