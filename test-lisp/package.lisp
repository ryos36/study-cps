;;; Sinby Corp. 2016

(in-package :cl-user)

(defpackage :sinby.cps.test
  (:use :cl :ext) ; :ext for string-concat
  (:nicknames :cps-test)
  (:export
    :*test-env*

    :*test-script-dir*
    :*test-ext*
    :*test-name*
    :*test-result-dir*
    :*test-files*
    :*test-parse-func*
    :*test-save*
    :*test-print*
    :*test-reset-func*
    :*test-success-n*
    :*test-save-n*
    :*test-insn-view*

    :set-test-files
    :do-test))
