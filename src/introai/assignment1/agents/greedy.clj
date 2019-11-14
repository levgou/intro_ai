(ns introai.assignment1.agents.greedy
  (:gen-class)
  (:require [introai.assignment1.operators :as op]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            [loom.alg :as alg]
            ))


(defn shortest-path-to-any [graph start-node destinations]
  (log/debug (<< "shortest path from ~{start-node} to any of ~{destinations}"))
  (let [shortest (apply min-key second (map #(alg/dijkstra-path-dist graph start-node %) destinations))]
    (first shortest)))

(defn next-node [graph-desc state targets]
  (let [next (second (shortest-path-to-any (:structure graph-desc)
                                           (:agent-node state)
                                           targets))]
    (log/debug (<< "next node is ~{next}"))
    next))

(defn node-on-way-to-people [graph-desc state]
  (log/debug "looking for people to save")
  (if (empty? (:remaining-people state))
    nil
    (next-node graph-desc state (keys (:remaining-people state)))))

(defn node-on-way-to-shelter [graph-desc state]
  (let [shelters (-> graph-desc :props :shelters)]
    (if (empty? shelters)
      nil
      (next-node graph-desc state shelters))))

(defn find-next-node [graph-desc state]
  (log/debug (<< "searching next node for ~{(:agent-node state)}"))

  (if (zero? (:carrying state))
    (node-on-way-to-people graph-desc state)
    (node-on-way-to-shelter graph-desc state)))

(defn greedy [graph-desc state]
  (let [next-node (find-next-node graph-desc state)]
    (log/debug "G - next node is " next-node)
    (if (nil? next-node)
      op/term
      (op/partial-edge graph-desc state next-node))))
