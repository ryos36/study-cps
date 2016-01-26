;;; Sinby Corp. 2016

(in-package :cl-user)

;----------------------------------------------------------------
(defpackage :sinby.cps.spill
  (:use :cl :cps-parser)
  (:import-from :cps-parser :cps-symbolp)
  (:import-from :cps-parser :cps-gensym)
  (:nicknames :cps-spill)

  (:export 
    :cps-spill 
    :make-new-spill-list
    :update-next-spill-list
    ))

