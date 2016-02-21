;
; Sinby Corp. 2016
;

(in-package :cl-user)

(defpackage :sinby.cps.resources
  (:use :cl)
  (:nicknames :cps-resources)
  (:export
    :cps-resources

    :add-global-function 
    :add-global-variable 

    :global-variables
    :global-functions

    :debug-mode

    :set-debug-mode
    :debug-mode?))
