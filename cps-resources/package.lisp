;
; Sinby Corp. 2016
;

(in-package :cl-user)

(defpackage :sinby.cps.resources
  (:use :cl)
  (:nicknames :cps-resources)
  (:export
    :cps-resources

    :debug-mode

    :set-debug-mode
    :debug-mode?))
