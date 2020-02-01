(ns introai.assignment4.graph-description
  (:gen-class)
  (:require [introai.assignment4.game-state :as gs]
            [introai.utils.const :refer [INF]]
            ))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight block-proba blocked]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method EdgeInfo [x ^java.io.Writer w] (.write w (str x)))


(defrecord GraphProps [num-nodes nodes edges])

(defrecord GraphDescription [structure props init-people]
  Object
  (toString [x] (str (-> x :props :edges))))
(defmethod print-method GraphDescription [x ^java.io.Writer w] (.write w (str x)))

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

(defn people-num [state]
  (or
    ((:remaining-people state) (:agent-node state))
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

(defn all-people [state]
  (->> state :remaining-people vals (apply +)))

(defn node-dead-line [graph-desc node]
  (-> graph-desc :props :nodes (get node) :dead-line))

