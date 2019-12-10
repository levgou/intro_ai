(ns introai.assignment2.graph-description
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.utils.log :as log]
            [loom.graph :as graph]
            [introai.utils.graphs :as gutils]
            [introai.utils.const :refer [INF]]
            ))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphProps [num-nodes shelters nodes edges])

(defrecord GraphDescription [structure props remaining-people])

(defrecord DenseGraphDescription [structure props remaining-people dense])

(defn state-node-info [graph-desc state]
  (-> graph-desc :props :nodes (#(% (:agent-node state)))))

(defn state-node-info-piece [graph-desc state key]
  (let [state-node (state-node-info graph-desc state)]
    (key state-node)))

(defn time-over? [graph-desc di-state agent]
  (let [deadline (state-node-info-piece graph-desc (gs/state-of di-state agent) :dead-line)]
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

(defn node-dead-line [graph-desc node]
  (-> graph-desc :props :nodes (get node) :dead-line))

(defn node-info-for-mid-edge-nodes [graph-desc]
  (let [mid-nodes (gutils/mid-nodes graph-desc)]
    (map #(map->NodeInfo {
                          :name %
                          :dead-line INF
                          :num-persons 0
                          :has-shelter false
                          }
                    ) mid-nodes)))

(defn node-info-for-mid-edge-edges [graph-desc]
  (let [mid-edges (gutils/mid-edges graph-desc)]
    (map #(map->EdgeInfo {
                          :name (str %)
                          :start (first %)
                          :end (second %)
                          :weight 1
                          }) mid-edges)))

(defn props-for-dense
  [{{shelters :shelters node-infos :nodes edge-infos :edges} :props
    :as graph-desc}]

  (let [new-num-nodes (count (graph/nodes (:structure graph-desc)))
        new-node-infos (merge node-infos (node-info-for-mid-edge-nodes graph-desc))
        new-edge-infos (merge edge-infos (node-info-for-mid-edge-edges graph-desc))]
    (GraphProps. new-num-nodes shelters new-node-infos new-edge-infos)))

(defn make-dense
  [{remaining-people :remaining-people :as graph-desc}]

  (let [new-graph-struct (gutils/dense-graph (:structure graph-desc))
        new-graph-props (props-for-dense new-graph-struct)]
    (DenseGraphDescription. new-graph-struct new-graph-props remaining-people true)))