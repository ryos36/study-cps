(load "package.lisp")
(load "resource-scheduler.lisp")

(use-package :resource-scheduler )

(setf scheduler (make-instance 'resource-scheduler))

(add-resource scheduler 'acc)
(add-resource scheduler (make-instance 'resource :name 'mem-ex :cost-value 3))
(add-resources scheduler '(r0 r1 r2 r3))
(add-resources scheduler '(r0 r1 r2 r4))
;(register-resource scheduler '(ex-r0 (0 3)))
(setf r0 (get-resource scheduler 'r0))
;(activate-resource scheduler r0)
(print `(resources ,(resources scheduler)))

(setf rx (make-instance 'resource :name :rx))

(print rx)
;(activate-resource scheduler rx)
(activate-resource scheduler 'acc)
(activate-resource scheduler 'mem-ex)
;(activate-resource scheduler 'r20)

(print r0)
(print (get-resource scheduler 'r2))

(setf node-0 (make-instance 'node :name 'node-0 ))
(add-node scheduler node-0 '(r0 r1) 'r2 'mem-ex)

(setf node-1 (make-instance 'node))
(add-node scheduler node-1 '(r2 r0) 'r3 'acc)

(print node-0)
(print node-1)

(print scheduler)

(build-connection scheduler)
(print (initial-nodes scheduler))
(print (final-nodes scheduler))
(print (is-dag? scheduler))
(print (slot-value scheduler 'dag-flag))
(print (is-dag? scheduler))

(print (initialize-ready-nodes scheduler))
(print (resources scheduler))
(print (update-ready-nodes scheduler))
(print (nodes scheduler))
(setf sc-node (select-candidate-node-to-ready scheduler))

(print (mapcar #'(lambda (n) `(,(name n) ,(get-cost n))) (nodes scheduler)))

(print `(here ,sc-node))
(apply-node scheduler sc-node)
(print (nodes scheduler))
(print (resources scheduler))
(update-accounting scheduler 3)
(print `(resources ,(resources scheduler)))
(print (update-ready-nodes scheduler))
(print `(xnodes ,(nodes scheduler)))
(print `(ready-nodes ,(ready-nodes scheduler)))
(print (is-finished? scheduler))

(setf sc-node (select-candidate-node-to-ready scheduler))
(print `(2-sc-node ,sc-node))
(print `(nodes ,(nodes scheduler)))

(apply-node scheduler sc-node)
(update-accounting scheduler)
(update-ready-nodes scheduler)
(print `(nodes ,(nodes scheduler)))

(print (is-finished? scheduler))
