;----------------------------------------------------------------
(load "../cps-resources/package.lisp")
(load "../cps-resources/cps-resources.lisp")

;----------------------------------------------------------------
(load "../cps-transfer/package.lisp")
(load "../cps-transfer/lisp-to-cps.lisp")
(load "../cps-transfer/make-cxr-route.lisp")
(load "../cps-transfer/primitive.lisp")

;----------------------------------------------------------------
(load "../eta-reduction/package.lisp")
(load "../eta-reduction/cps.lisp")
(load "../eta-reduction/optimize.lisp")

;----------------------------------------------------------------
(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

(load "../k-transfer/free-variable-finder.lisp")
(load "../k-transfer/closure-converter.lisp")

;----------------------------------------------------------------
(load "../resource-scheduler/package.lisp")
(load "../resource-scheduler/resource-scheduler.lisp")

;----------------------------------------------------------------
(load "../cps-reorder/package.lisp")
(load "../cps-reorder/vm-scheduler.lisp")
(load "../cps-reorder/cps-block-analyzer.lisp")
(load "../cps-reorder/cps-reorder.lisp" )

;----------------------------------------------------------------
(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

;----------------------------------------------------------------
(load "../cps-spill/package.lisp")
(load "../cps-spill/cps-spill.lisp" )

;----------------------------------------------------------------
(load "../vm-codegen/package.lisp")
(load "../vm-codegen/vm-codegen.lisp" )
(load "../vm-codegen/heap-parser.lisp" )

;----------------------------------------------------------------
(load "../vmgen/package.lisp")
(load "../vmgen/vmgen.lisp")
(load "../vmgen/vmc-to-bin.lisp")

;----------------------------------------------------------------
(use-package :cps-resources)

(use-package :cps-transfer)
;(use-package :cps-eta-reduction)
;(use-package :cps-parser)
(use-package :cps-free-variable-finder)
(use-package :cps-closure-converter)
(use-package :cps-spill)
(use-package :vm-codegen)

;----------------------------------------------------------------
(defparameter *cps-resources* (make-instance 'cps-resources))
(setf use-exit-primitive nil)

;----------------------------------------------------------------
(defparameter *transfer-table* (make-transfer-table))

;----------------------------------------------------------------
(setf reorder (make-instance 'cps-reorder:cps-reorder :sym-name "ro-sym"))

;----------------------------------------------------------------
(setf conveter (make-instance 'cps-closure-converter:closure-converter :sym-name "k-sym"))

;----------------------------------------------------------------
(setf spill (make-instance 'cps-spill:cps-spill :sym-name "s-sim" :max-n 10))

(defun cps-spill-parse-one (spill cps-expr env)
  (let* ((finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
         (finder-env (cps-parser:make-new-env finder '() '()))
         (result (cps-parser:cps-parse finder cps-expr finder-env))
         (spill-env (cps-parser:make-new-env spill '() 
                                  (copy-tree `((:live-vars ,@result)
                                               (:spill
                                                 (:used )
                                                 (:duplicate 0)
                                                 (:spill-out ))))
                                  )))

    (cps-parser:cps-parse spill cps-expr spill-env)))

;----------------------------------------------------------------
(defparameter *codegen* nil)

(defun cps-codegen-parse-one (cps-expr env)
  (let* ((finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
         (codegen (make-instance 'vm-codegen:vm-codegen :live-variables-finder finder :sym-name "label"))
         (finder-env (cps-parser:make-new-env finder '() '()))
         (result (cps-parser:cps-parse finder cps-expr finder-env))
         (codegen-env (cps-parser:make-new-env codegen '()
                                    (copy-tree `((:live-vars ,result)
                                                 (:codegen
                                                   (:register ,(make-list (max-n codegen)))
                                                   (:app-info)))))))
    (setf *codegen* codegen)

    (cps-parser:cps-parse codegen cps-expr codegen-env)
    (create-initialize-codes codegen)
    (get-final-codes codegen)))

;----------------------------------------------------------------
(defparameter vmgen (make-instance 'cps-vmgen:vmgen))
(defparameter *bin-file-name* nil)

(defun vmgen-one (codes env)
  (mapcar #'(lambda (expr)
              (cps-vmgen:convert vmgen expr)) codes)
  (if *bin-file-name*
    (cps-vmgen:write-binary-with-open-file vmgen *bin-file-name* '(unsigned-byte 8)))

  ;(if (find :vmgen *debug-modes*)

  (if (debug-mode? *cps-resources* :vmgen)
    (debug-print-cps
      (cps-vmgen:get-codes vmgen)
      '(:vmgen)))
  codes)

;----------------------------------------------------------------
(defparameter *debug-modes* nil)
(defun debug-print-cps (expr env)

  (when (find :list *debug-modes*)
    (format t "~%")
    (format t ":READ~%")
    (mapc #'(lambda (x) (if (eq (car x) #'debug-print-cps)
                         (format t "~s~%" (cadr x)))) func-env-pair)
    (format t ":VMGEN~%")
    (exit))

  (if (find :process *debug-modes*)
    (print env))
  (when (find (car env) *debug-modes*)
    (print `(,(car env) ,expr))
    (if (find :stop *debug-modes*)
      (exit)))
  expr)

;----------------------------------------------------------------
(defmethod print-codes (codes &optional (str t))
  (let ((prefix-str "("))
    (dolist (i codes)
      (format str "~a~s" prefix-str i)
      (setf prefix-str "- ")
      (setf (elt prefix-str 0) #\newline)))
  (format str ")~%"))

;----------------------------------------------------------------
(setf func-env-pair `((,#'do-lisp-list-to-cps . ,(make-exit-continuous use-exit-primitive))
                      (,#'debug-print-cps . (:lisp-to-cps :eta-reduction))
                      (,#'cps-eta-reduction:walk-cps . ,(cps-eta-reduction:make-env))
                      (,#'debug-print-cps . (:eta-reduction :closure-converter))
                      ((,#'cps-parser:cps-parse . ,conveter) .  ,(cps-parser:make-new-env conveter '()))
                      (,#'debug-print-cps . (:closure-converter :reorder))
                      ((,#'cps-parser:cps-parse . ,reorder) .  ,(cps-parser:make-new-env reorder '()))
                      (,#'debug-print-cps . (:reorder :spill))
                      ((,#'cps-spill-parse-one . ,spill) . nil)
                      (,#'debug-print-cps . (:spill :codegen))
                      (,#'cps-codegen-parse-one . nil)
                      (,#'debug-print-cps . (:codegen :vmgen))
                      (,#'vmgen-one . nil)
                      ))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun this-usage () (format *error-output* "~%Usage:clisp cps-compiler.lisp [-d] <scm file>~%"))

;----------------------------------------------------------------
(defun check-option (option-list) 
  (let ((o (car option-list)))
    (if (> (length o) 1)
      (let ((c0 (elt o 0))
            (c1 (elt o 1))
            (str2- (string-upcase (substring o 2))))
        (when (eq c0 #\-)
          (if (= (length str2-) 0)
            (push :process *debug-modes*)
            (flet ((push-w (key0)
                     (push key0 *debug-modes*)
                     (set-debug-mode *cps-resources* key0)))
              (let ((key (intern 
                           (if (eq (elt str2- 0) #\:) 
                             (substring str2- 1)
                             str2-) :keyword)))
                (push-w key))))

          (check-option (cdr option-list)))))))

;----------------------------------------------------------------
(let ((av (argv))
      (ext-str ".scm"))

  (let* ((av-len (length av))
         (last-arg (elt av (- av-len 1)))
         (last-arg-len (length last-arg))
         (ext-str-len (length ext-str))
         (tiny-scheme-file0
           (if (string= ext-str
                        (subseq last-arg (- last-arg-len ext-str-len)))
             last-arg
             (concatenate 'string last-arg ".scm"))))
    (when tiny-scheme-file0
      (setf tiny-scheme-file tiny-scheme-file0)
      (setf output-file-name (concatenate 'string (subseq tiny-scheme-file0 0 (- (length tiny-scheme-file0) ext-str-len)) ".vmc"))
      (setf *bin-file-name* (concatenate 'string (subseq tiny-scheme-file0 0 (- (length tiny-scheme-file0) ext-str-len)) ".vmb"))
      (check-option (cdr (reverse (map 'list #'(lambda (x) x) av))))
      (print `(,tiny-scheme-file :-> ,output-file-name)))))
;----------------------------------------------------------------

(if (and tiny-scheme-file (probe-file tiny-scheme-file))
  (let ((tiny-scheme-program
          (with-open-file (in tiny-scheme-file)
            (labels ((read-all (rv)
                       (let ((read-one (read in nil)))
                         (if (null read-one) (nreverse rv)
                            (read-all (push read-one rv))))))
                 (read-all nil)))))

    (if (debug-mode? *cps-resources* :read)
      (debug-print-cps tiny-scheme-program '(:read :lisp-to-cps)))

    ;(print-cps tiny-scheme-program :tiny-scheme-file)
    (labels ((proc-loop (func-env-pair0 expr)
                (if (null func-env-pair0) expr
                  (let ((func (caar func-env-pair0))
                        (env (cdar func-env-pair0)))
                    (if (listp func)
                      (let ((method (car func))
                            (instance (cdr func)))
                        (proc-loop (cdr func-env-pair0) (funcall method instance expr env)))

                      (proc-loop (cdr func-env-pair0) (funcall func expr env)))))))


      (let ((codes (proc-loop func-env-pair tiny-scheme-program)))
        (with-open-file (out output-file-name :if-does-not-exist :create :direction :output)
          (print-codes codes out)))))

  (progn
    (format *error-output* "~a is not exist.~%" tiny-scheme-program)
    (this-usage)))

