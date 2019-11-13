(ns introai.assignment1.graph-description
  (:require [ubergraph.core :as uber]))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphDescription [structure props])

(defn state-node-info [graph-desc state]
  (-> graph-desc :props :nodes (#(% (:agent-node state)))))

(defn state-node-info-piece [graph-desc state key]
  (let [state-node (state-node-info graph-desc state)]
    (key state-node)))

(defn time-over? [graph-desc state]
  (let [deadline (state-node-info-piece graph-desc state :dead-line)]
    (> (:time state) deadline)))

(defn shelter? [graph-desc state]
  (state-node-info-piece graph-desc state :has-shelter))

(defn people-num [graph-desc state]
  (state-node-info-piece graph-desc state :num-persons))

(defn non-empty-nodes [graph-desc]
  (remove #(zero? (:num-persons %)) (-> graph-desc :props :nodes vals)))

(defn people-map [graph-desc]
  (let [non-zero-nodes (non-empty-nodes graph-desc)]
    (apply hash-map
           (apply concat
                  (map #(vals (select-keys % [:name :num-persons])) non-zero-nodes)))))

(defn find-edge [graph-desc v1 v2]
  (let [struct (:structure graph-desc)]
    (into {:weight (uber/weight struct v1 v2)} (uber/find-edge struct v1 v2))))