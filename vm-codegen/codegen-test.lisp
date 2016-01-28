(load "../k-transfer/package.lisp")
(load "../k-transfer/cps-parser.lisp")
(load "../k-transfer/utils.lisp")

(load "../cps-live-variables-finder/package.lisp")
(load "../cps-live-variables-finder/cps-live-variables-finder.lisp")

(load "package.lisp")
(load "vm-codegen.lisp" )

(load "../test-lisp/package.lisp")
(load "../test-lisp/test.lisp")

(use-package :cps-parser)
(use-package :vm-codegen)
(use-package :cps-test)

(setf codegen (make-instance 'vm-codegen:vm-codegen))
(setf finder (make-instance 'cps-live-variables-finder:cps-live-variables-finder))
(setf *test-env* (make-new-env codegen '()))

(defun cps-parse-one (cps-expr env)
  (let* ((finder-env (make-new-env finder '() '()))
         (result (cps-parse finder cps-expr finder-env))
         (codegen-env (make-new-env codegen '()
                                    (copy-tree `((:live-vars ,@result)
                                                 (:codegen
                                                   (:register (make-list (max-n codegen)))
                                                   (:app-info)))))))
    (cps-parse codegen cps-expr codegen-env)))

(defparameter *test-script-dir* "../cps-script/" )
(defparameter *test-ext* ".cps")
(defparameter *test-parse-func* #'cps-parse-one)
(defparameter *debug-mode* nil)
(defparameter *debug-mode* t)
(defun cps-gensym-reset ()
  (setf (slot-value codegen 'sym-no) 0))

(defparameter *test-reset-func* #'cps-gensym-reset)

(print
(find-app-for-branch-prediction 
  codegen
'(:FIXH
 ((:BIND (:DECLARE |zym2| J0 I A B) (:USE) (:LIVE I)
   ((:FIXS
     ((:BIND (:DECLARE |zym4|) (:USE) (:LIVE)
       ((:APP (:DECLARE) (:USE |zym4|) (:LIVE)))))
     (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
      ((:= (:DECLARE) (:USE) (:LIVE I)
        ((:APP (:DECLARE) (:USE :NO-SYMBOL) (:LIVE))
         (:+ (:DECLARE |zym9|) (:USE I) (:LIVE)
          ((:FIXS
            ((:BIND (:DECLARE |zym8|) (:USE) (:LIVE)
              ((:FIXS
                ((:BIND (:DECLARE |zym6|) (:USE) (:LIVE)
                  ((:APP (:DECLARE) (:USE |zym6|) (:LIVE)))))
                (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
                 ((:APP (:DECLARE) (:USE |zym5| J0 |zym9|) (:LIVE))))))))
            (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
             ((:APP (:DECLARE) (:USE |zym7| J0 I) (:LIVE)))))))))))))))
 (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
  ((:+ (:DECLARE C) (:USE B) (:LIVE A C)
    ((:* (:DECLARE F) (:USE D E) (:LIVE F)
      ((:+ (:DECLARE I) (:USE G H) (:LIVE I)
        ((:* (:DECLARE L) (:USE J K) (:LIVE L)
          ((:+ (:DECLARE O) (:USE M N) (:LIVE O)
            ((:+ (:DECLARE P) (:USE C F) (:LIVE P)
              ((:+ (:DECLARE Q) (:USE I L) (:LIVE Q)
                ((:* (:DECLARE R) (:USE O P) (:LIVE R)
                  ((:* (:DECLARE S) (:USE R Q) (:LIVE S)
                    ((:+ (:DECLARE T) (:USE A S) (:LIVE)
                      ((:APP (:DECLARE) (:USE T)
                        (:LIVE)))))))))))))))))))))))))))

(print
(find-app-for-branch-prediction 
  codegen
'(:BIND (:DECLARE |zym2| J0 I A B) (:USE) (:LIVE I)
   ((:FIXS
     ((:BIND (:DECLARE |zym4|) (:USE) (:LIVE)
       ((:APP (:DECLARE) (:USE |zym4|) (:LIVE)))))
     (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
      ((:= (:DECLARE) (:USE) (:LIVE I)
        ((:APP (:DECLARE) (:USE :NO-SYMBOL) (:LIVE))
         (:+ (:DECLARE |zym9|) (:USE I) (:LIVE)
          ((:FIXS
            ((:BIND (:DECLARE |zym8|) (:USE) (:LIVE)
              ((:FIXS
                ((:BIND (:DECLARE |zym6|) (:USE) (:LIVE)
                  ((:APP (:DECLARE) (:USE |zym6|) (:LIVE)))))
                (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
                 ((:APP (:DECLARE) (:USE |zym5| J0 |zym9|) (:LIVE))))))))
            (:FIX-BODY (:DECLARE) (:USE) (:LIVE)
             ((:APP (:DECLARE) (:USE |zym7| J0 I) (:LIVE))))))))))))))))
