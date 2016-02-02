;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-codegen
  (:use :cl :cps-parser :cps-live-variables-finder)
  (:nicknames :vm-codegen)

  (:export 
    :vm-codegen

    :max-n
    :global-variable

    :add-global-variable
    :global-variable? 

    :get-final-codes 
    :print-codes

    :find-app-for-branch-prediction ))

