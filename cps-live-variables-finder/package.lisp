;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.live-variables-finder
  (:use :cl :cps-parser)
  (:nicknames :cps-live-variables-finder)

  (:export 
    :cps-live-variables-finder 
    ))

