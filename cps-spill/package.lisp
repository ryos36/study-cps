;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.spill
  (:use :cl :cps-parser :cps-parser)
  (:nicknames :cps-spill)

  (:export 
    :cps-spill 
    :make-new-spill-list
    :update-next-spill-list

    :create-reference-wrapper
    :create-stack-wrapper
    :create-pop-wrapper ))

