(ns introai.assignment2.graph-description
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.utils.log :as log]))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphProps [num-nodes shelters nodes edges])

(defrecord GraphDescription [structure props remaining-people])

(defn state-node-info [graph-desc state]
  (-> graph-desc :props :nodes (#(% (:agent-node state)))))

(defn state-node-info-piece [graph-desc state key]
  (let [state-node (state-node-info graph-desc state)]
    (key state-node)))

(defn time-mod [di-state agent]
  (let [time-modifier (gs/state-piece-of di-state agent :time-modifier)]
    (log/debug "Time modifier of - " (:name agent) " - " time-modifier)
    time-modifier))

(defn time-over? [graph-desc di-state agent]
  (let [deadline (+ (state-node-info-piece graph-desc (gs/state-of di-state agent) :dead-line)
                    (time-mod di-state agent))]
    (> (:time di-state) deadline)))

(defn shelter? [graph-desc state]
  (state-node-info-piece graph-desc state :has-shelter))

(defn people-num [graph-desc state]
  (or
    ((:remaining-people graph-desc) (:agent-node state))
    0)
  ;(state-node-info-piece graph-desc state :num-persons)
  )

(defn non-empty-nodes [nodes-map]
  (remove #(zero? (:num-persons %)) (vals nodes-map)))

(defn people-map [nodes-map]
  (let [non-zero-nodes (non-empty-nodes nodes-map)]
    (apply hash-map
           (apply concat
                  (map #(vals (select-keys % [:name :num-persons])) non-zero-nodes)))))

(defn remaining-nodes [{ppl-map :remaining-people}]
  (keys ppl-map))

(defn all-people [graph-desc]
  (->> graph-desc :remaining-people vals (apply +)))
