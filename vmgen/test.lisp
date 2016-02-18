(load "package.lisp")
(load "vmgen.lisp")
(load "vmc-to-bin.lisp")

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-vmgen)
(use-package :cps-test)

(setf vmgen (make-instance 'vmgen))
;(setf converter (make-instance 'vmc-to-c-source :vmgen vmgen))
(setf *test-env* nil)

;----------------------------------------------------------------

;----------------------------------------------------------------

(defun vmgen-one (code-tagged-list env)
  (mapcar #'(lambda (expr)
              (convert vmgen expr))
          (cdr code-tagged-list))
  (write-binary-with-open-file vmgen (concatenate 'string *test-result-dir* *test-current-test-name* ".vmb") '(unsigned-byte 8))
  (get-codes vmgen))

;----------------------------------------------------------------
(defparameter *test-script-dir* "./codes/" )
(defparameter *test-ext* ".vmc")
(defparameter *test-parse-func* #'vmgen-one)
(defparameter *debug-mode* nil)
(defparameter *test-src-insn-view* t)
(defparameter *test-insn-view* t)

;(print (insn-pos-pair vmgen))
;(print (address-pos-pair vmgen))
;(print (label-offset-pos-pair vmgen))
;(print (label-pos-pair vmgen))

#|
(let ((n 0)
      (prefix ""))
  (dolist (i (to-binary-list vmgen))
    (format t "~a~2,'0x" prefix i)
    (incf n)
    (setf prefix (if (eq (mod n 16) 0) #\newline " "))))

|#

;(set-test-files '("32" "42" "29" "41"))
(set-test-files '("3" (1 . 2)))
(do-test)
