;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.vm-codegen
  (:use :cl :cps-parser :cps-live-variables-finder :csf-resources)
  (:nicknames :vm-codegen)

  (:export 
    :vm-codegen

    :max-n
    :global-variable

    :reset-codes 

    :add-global-variable
    :global-variable? 

    :create-initialize-codes 
    :get-final-codes 
    :print-codes

    :find-app-for-branch-prediction ))

