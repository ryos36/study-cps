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
    :make-new-env
    :cps-primitive-p
    :cps-error-exit
    :cps-parse))
