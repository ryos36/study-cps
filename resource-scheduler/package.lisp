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
    :ready-nodes

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

    :dag-flag

    :name

    :add-successor

    :cost-value
    :get-resource
    :add-resource
    :add-resources
    :add-node
    :build-connection
    :is-dag?
    :initialize-ready-nodes
    :initialize-activate-resources
    :set-ready
    :reset-ready-nodes
    :update-ready-nodes
    :select-candidate-node-to-ready
    :apply-node
    :update-accounting
    :activate-resource
    :is-finished?
    :ordered-nodes
    :get-cost
    :update-cost
    :get-resource-size
    :get-all-resources
    :set-status
    :update-accounting))
