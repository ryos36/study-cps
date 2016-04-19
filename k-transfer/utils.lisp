;----------------------------------------------------------------
(in-package :sinby.cps.parser)

(defun make-cxr-route (expr word)
  (labels ((make-cxr0 (expr0 result)
              (if (null expr0) nil
                (let ((result-l (cons 'a result)))
                  (if (eq expr0 word) result
                    (if (atom expr0) nil
                      (let ((first-expr0 (car expr0))
                            (remain-expr0  (cdr expr0))
                            (result-r (cons 'd result)))
                        (let ((rv (make-cxr0 first-expr0 result-l)))
                          (if rv rv
                            (make-cxr0 remain-expr0 result-r))))))))))

    (make-cxr0 expr '())))


(defun pickup-list (expr word &optional debug-print)
  (labels ((do-cxr (cxr expr0)
                   (if (null cxr) expr0
                     (if (atom expr0)
                       expr0
                       (let* ((op (if (eq 'a (car cxr)) #'car #'cdr))
                              (rv (apply op (list expr0))))
                         (if debug-print (print rv))
                         (do-cxr (cdr cxr) rv))))))

    (let ((rv-cxr (make-cxr-route expr word)))
      (if debug-print (print rv-cxr))
      (do-cxr (reverse (cdr rv-cxr)) expr))))

;----------------------------------------------------------------
(defun closure-name-to-label-name (closure-name)
  (intern (format nil ":~s" closure-name)))

(defun make-label-address (sym &optional c-to-l)
  `(:ADDRESS ,(if c-to-l (closure-name-to-label-name sym) sym)))

(defun make-registers (n &optional (prefix :R) &key (format-func #'(lambda(no prefix) (intern (format nil "~a~a" prefix no) "KEYWORD" ))))
  (labels ((make-registers0 (n0 rv)
              (if (= n0 n) (nreverse rv)
                (make-registers0 (+ n0 1)
                   (cons (funcall format-func n0 prefix) rv)))))
    (make-registers0 0 '())))
