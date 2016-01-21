;;; Sinby Corp. 2016

(in-package :cl-user)

(defpackage :sinby.cps.resource-scheduler
  (:use :cl)
  (:nicknames :resource-scheduler)
  (:export
    :resource-scheduler

    :resources
    :nodes
    :initial-nodes
    :final-nodes
    :runnable-nodes

    :node

    :instruction
    :status
    :input-resources
    :output-resources
    :special-resources
    :successors

    :resource
    ; :status
    :ccounting

    :get-resource
    :register-resource
    :register-resources
    :add-node
    :build-connection
    :is-dag?
    :initialize-runnable-nodes
    :set-runnable
    :reset-runnable-nodes
    :update-runnable-nodes
    :select-candidate-node-to-run
    :run-node
    :update-accounting
    :activate-resource
    :is-finished?
    :get-cost
    :update-cost
    :get-resource-size
    :get-all-resources
    :set-status
    :update-accounting))
