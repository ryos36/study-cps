;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.eta-reduction
  (:use :cl )
  (:nicknames :cps-eta-reduction)

  (:export 
    :walk-cps
    :make-env

    :*debug-mode*
    :*cps-stack*
    :*env*
    :*cps-exit-output*))

