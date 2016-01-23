;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-scheduler
  (:use :cl :sinby.cps.resource-scheduler)
  (:import-from :cps-parser :cps-symbolp)
  (:nicknames :vm-scheduler)

  (:export 
    :vm-scheduler 
    :resources

    :costs

    :cps-expr

    :add-vm-register
    :add-vm-registers
    :get-special-resource
    :add-primitive-instruction
    :add-apply-instruction
    :get-cost ))

;----------------------------------------------------------------
(defpackage :sinby.cps.reorder
  (:use :cl :sinby.cps.parser :sinby.cps.resource-scheduler :sinby.cps.vm-scheduler)
  (:nicknames :cps-reorder)

  (:export 

    :cps-reorder 
    :cps-bind 
    :cps-primitive 

    ))
