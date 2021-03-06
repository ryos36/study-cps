;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-instruction
  (:use :cl :sinby.cps.node)
  (:nicknames :vm-instruction)
  (:export 
    :vm-instruction

    :cps-expr))

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-scheduler
  (:use :cl :sinby.cps.resource-scheduler :sinby.cps.vm-instruction)
  (:import-from :cps-parser :cps-symbolp)
  (:nicknames :vm-scheduler)

  (:export 
    :vm-scheduler 
    :resources

    :costs

    :gogo

    :add-vm-register
    :add-vm-registers
    :get-special-resource
    :add-primitive-instruction
    :add-apply-instruction
    
    :eval-alloc-cost))


;----------------------------------------------------------------
(defpackage :sinby.cps.reorder
  (:use :cl :sinby.cps.parser :sinby.cps.resource-scheduler :sinby.cps.vm-scheduler :sinby.cps.vm-instruction)
  (:nicknames :cps-reorder)

  (:export 

    :cps-reorder 
    :cps-bind 
    :cps-primitive 

    ))
