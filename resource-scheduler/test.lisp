(load "resource-scheduler.lisp")

(setf scheduler (make-instance 'resource-scheduler))

(register-resource scheduler 'acc)
(register-resource scheduler (make-instance 'resource :name 'mem-ex :cost-value 3))
(register-resources scheduler '(r0 r1 r2 r3))
;(register-resource scheduler '(ex-r0 (0 3)))
(setf r0 (get-resource scheduler 'r0))
;(activate-resource scheduler r0)

(setf rx (make-instance 'resource :name :rx))

(print rx)
;(activate-resource scheduler rx)
(activate-resource scheduler 'acc)
(activate-resource scheduler 'mem-ex)
;(activate-resource scheduler 'r20)

(print r0)
(print (get-resource scheduler 'r2))

(setf node-0 (make-instance 'node :name 'node-0 ))
(add-node scheduler node-0 '(r0 r1) 'mem-ex 'r2)

(setf node-1 (make-instance 'node))
(add-node scheduler node-1 '(r2 r0) 'acc 'r3)

(print node-0)
(print node-1)

(print scheduler)

(build-connection scheduler)
(print (initial-nodes scheduler))
(print (final-nodes scheduler))
(print (is-dag? scheduler))
(print (slot-value scheduler 'dag-flag))
(print (is-dag? scheduler))

(print (initialize-runnable-nodes scheduler))
(print (reverse (resources scheduler)))
(print (update-runnable-nodes scheduler))
(print (nodes scheduler))
(setf sc-node (select-candidate-node-to-run scheduler))

(print (mapcar #'(lambda (n) `(,(name n) ,(get-cost n))) (nodes scheduler)))

(print `(here ,sc-node))
(run-node scheduler sc-node)
(print (nodes scheduler))
(print (reverse (resources scheduler)))
(update-accounting scheduler 3)
(print (reverse (resources scheduler)))
(print (update-runnable-nodes scheduler))
(print `(nodes ,(nodes scheduler)))
(print `(runnable-nodes ,(runnable-nodes scheduler)))
(print (is-finished? scheduler))

(setf sc-node (select-candidate-node-to-run scheduler))
(print `(sc-node ,sc-node))
(print `(nodes ,(nodes scheduler)))
(run-node scheduler sc-node)
(update-accounting scheduler)
(update-runnable-nodes scheduler)
(print (nodes scheduler))
(print (is-finished? scheduler))
