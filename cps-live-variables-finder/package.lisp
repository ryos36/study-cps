;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.live-variables-finder
  (:use :cl :cps-parser)
  (:import-from :cps-parser :cps-symbolp)
  (:nicknames :cps-live-variables-finder)

  (:export 
    :cps-live-variables-finder 

    :get-vars
    :get-declare-tagged-list 
    ))

