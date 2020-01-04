(ns introai.assignment3.agents.greedy
  (:gen-class)
  (:require [introai.assignment3.operators :as op]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            [loom.alg :as alg]
            [introai.assignment3.game-state :as gs]))


(defn shortest-path-to-any [graph start-node destinations]
  (log/debug (<< "shortest path from ~{start-node} to any of ~{destinations}"))
  (let [shortest (apply min-key second (map #(alg/dijkstra-path-dist graph start-node %) destinations))]
    (log/debug (<< "Shortest path is: ~{(first shortest)} with weight: ~{(second shortest)}"))
    (first shortest)))

(defn next-node [graph-desc state targets]
  (let [next (second (shortest-path-to-any (:structure graph-desc)
                                           (:agent-node state)
                                           targets))]
    (log/debug (<< "next node is ~{next}"))
    next))

(defn node-on-way-to-people [graph-desc state]
  (log/debug "looking for people to save out of: " (:remaining-people graph-desc))
  (if (empty? (:remaining-people graph-desc))
    nil
    (next-node graph-desc state (keys (:remaining-people graph-desc)))))

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

(defn greedy [graph-desc di-state agent]
  (let [next-node (find-next-node graph-desc (gs/state-of di-state agent))]
    (if (nil? next-node)
      (op/make-term (gs/state-piece-of di-state agent :agent-node))
      (op/make-edge graph-desc
                    (gs/state-of di-state agent)
                    next-node))))
