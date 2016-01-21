;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-scheduler
  (:use :cl :sinby.cps.resource-scheduler)
  (:nicknames :vm-scheduler)

  (:export 
    :vm-scheduler 
    :resources
    ))

;----------------------------------------------------------------
(defpackage :sinby.cps.reorder
  (:use :cl :sinby.cps.parser :sinby.cps.resource-scheduler)
  (:nicknames :cps-reorder)

  (:export 
    :cps-reorder 
    ))
