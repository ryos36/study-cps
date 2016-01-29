;;; Sinby Corp. 2016

(in-package :cl-user)

(defpackage :sinby.cps.parser
  (:use :cl)
  (:nicknames :cps-parser)
  (:export
    :cps-parser

    :def-cps-func

    :sym-no

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

    :make-cxr-route
    :pickup-list))
