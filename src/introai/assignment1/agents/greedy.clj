(ns introai.assignment1.agents.greedy
  (:gen-class)
  (:require [introai.assignment1.operators :as op]
            [ubergraph.alg :refer [shortest-path path-to cost-of-path start-of-path nodes-in-path]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            ))


(defn cost-and-path [p]
  [(cost-of-path p) p])

(defn shortest-path-to [shortest-paths destinations]
  (map #(path-to shortest-paths %) destinations))

(defn shortest-path-to-any [graph start-node destinations]
  (log/debug (<< "shortest path from ~{start-node} to any of ~{destinations}"))
  (let [shortest-paths (shortest-path graph {:start-node start-node :cost-attr :weight})]

    (log/debug "Found all shortest paths, now take shortest")
    (->
      destinations
      (#(shortest-path-to shortest-paths %))
      (#(map cost-and-path %))
      (#(apply min-key first %))
      second
      nodes-in-path
      )))

(defn next-node [graph-desc state targets]
  (let [next (second (shortest-path-to-any (:structure graph-desc)
                                           (:agent-node state)
                                           targets))]
    (log/debug (<< "next node is ~{next}"))
    next))

(defn node-on-way-to-people [graph-desc state]
  (if (empty? (:remaining-people state))
    nil
    (next-node graph-desc state (keys (:remaining-people state)))))

(defn node-on-way-to-shelter [graph-desc state]
  (next-node graph-desc state (:shelters graph-desc)))

(defn find-next-node [graph-desc state]
  (log/debug (<< "searching next node for ~{(:agent-node state)}"))

  (if (zero? (:carrying state))
    ((log/debug "looking for people to save")
     (node-on-way-to-people graph-desc state))

    ((log/debug "looking for shelter")
     (node-on-way-to-shelter graph-desc state))))

(defn greedy [graph-desc state]
  (let [next-node (find-next-node graph-desc state)]
    (if (nil? next-node)
      (op/term state)
      (op/partial-edge graph-desc state next-node))))
