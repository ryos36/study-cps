;----------------------------------------------------------------
(in-package :sinby.cps.eta-reduction)

;----------------------------------------------------------------
(defun eta-reduction (cps-expr)
  (flet ((replace-app-func (found-list0)
            (let ((old-func-name (car found-list0))
                  (func-name (cadr found-list0))
                  (cps-expr0 (caddr found-list0)))


                           )))
    (let* ((op (car cps-expr))
           (found-list
             (if (or (eq op :FIXS) (eq op :FIXH))
               (let* ((fbinds (cadr cps-expr))
                      (fbind (car fbinds)))
                 (if (null (cadr fbinds))
                   (let ((func-args (cadr fbind))
                         (func-body (caddr fbind)))
                     (if (null (cadr func-args))
                       (let ((func-body-op (car func-body))
                             (func-arg0 (car func-args)))
                         (if (eq func-body-op :APP)
                           (let ((func-name (car fbind))
                                 (new-func-name (cadr func-body))
                                 (new-func-args (caddr func-body)))
                             (if (null (cadr new-func-args))
                               (let ((new-func-arg0 (car new-func-args)))
                                 (if (eq func-arg0 new-func-arg0)
                                   `(,func-name ,new-func-name ,(copy-tree (caddr cps-expr))))))))))))))))
      (if found-list
        (replace-app-func found-list) 
        cps-expr))))

