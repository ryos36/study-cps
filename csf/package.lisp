;
; Sinby Corp. 2016
;
; Sinby Comiler 'Study Framework
;

(in-package :cl-user)

(defpackage :sinby.csf.resources
  (:use :cl)
  (:nicknames :csf-resources)
  (:export
    :csf-resources

    :add-global-function 
    :add-global-variable 

    :global-variables
    :global-functions

    :debug-mode

    :set-debug-mode
    :debug-mode?))

