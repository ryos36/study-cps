;
; Sinby Corp. 2016
;

(in-package :cl-user)

(defpackage :sinby.cps.parser
  (:use :cl)
  (:nicknames :cps-parser)
  (:export
    :cps-parser

    :def-cps-func

    :sym-no
    :sym-name

    :cps-gensym
    :cps-symbolp
    :make-new-env
    :cps-primitive-p
    :cps-error-exit

    :cps-terminal
    :cps-symbol
    :cps-bind
    :cps-binds
    :cps-fix
    :cps-fixh
    :cps-fixs
    :cps-app
    :cps-exit
    :cps-primitive
    
    :cps-parse

    :compare-primitivep 

    :closure-name-to-label-name 
    :make-label-address 
    :make-registers

    :make-cxr-route
    :pickup-list))

;----------------------------------------------------------------
(defpackage :sinby.cps.free-variable-finder
  (:use :cl :cps-parser)
  (:nicknames :cps-free-variable-finder)

  (:export 
    :free-variable-finder

    :filter-free-variables
    ))

;----------------------------------------------------------------
(defpackage :sinby.cps.closure-converter
  (:use :cl :cps-parser :cps-free-variable-finder)
  (:nicknames :cps-closure-converter)

  (:export 
    :closure-converter
    ))
