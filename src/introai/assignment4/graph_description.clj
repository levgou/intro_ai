(ns introai.assignment4.graph-description
  (:gen-class)
  (:require [introai.assignment4.game-state :as gs]
            [introai.utils.log :as log]
            [loom.graph :as graph]
            [introai.utils.graphs :as gutils]
            [introai.utils.const :refer [INF]]
            ))

(defrecord NodeInfo [name dead-line num-persons has-shelter flood-prob]
  Object
  (toString [gs] (str "NodeInfo: " (select-keys gs [:name :flood-prob]))))
(defmethod print-method NodeInfo [gs ^java.io.Writer w] (.write w (str gs)))

(defrecord EdgeInfo [name start end weight]
  Object
  (toString [gs] (str "EdgeInfo: " (into {} gs))))
(defmethod print-method EdgeInfo [gs ^java.io.Writer w] (.write w (str gs)))

(defrecord GraphProps [num-nodes persist nodes edges])

(defrecord GraphDescription [structure props remaining-people]
  Object
  (toString [x] (str "GraphDescription: " (select-keys x [:remaining-people]))))
(defmethod print-method GraphDescription [x ^java.io.Writer w] (.write w (str x)))

(defrecord DenseGraphDescription [structure props remaining-people dense]
  Object
  (toString [x] "DenseGraphDesc"))
(defmethod print-method DenseGraphDescription [x ^java.io.Writer w] (.write w (str x)))

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

(defn node-info-for-node-name [node-name]
  (map->NodeInfo {:name node-name :dead-line INF :num-persons 0 :has-shelter false}))

(defn node-info-for-mid-edge-nodes [graph-desc]
  (let [mid-nodes (gutils/mid-nodes graph-desc)
        node-infos (map node-info-for-node-name mid-nodes)]

    (zipmap (sort mid-nodes) (sort-by :name node-infos))))

(defn edge-info-for-edge [edge]
  (map->NodeInfo {:name (str edge) :start (first edge) :end (second edge) :weight 1}))

(defn edge-info-for-mid-edge-edges [graph-desc]
  (let [mid-edges (gutils/mid-edges graph-desc)
        edge-infos (map edge-info-for-edge mid-edges)]

    (zipmap (sort (map :name edge-infos)) (sort-by :name edge-infos))))

(defn props-for-dense
  [{{shelters :shelters node-infos :nodes edge-infos :edges} :props}
   new-graph-struct]

  (let [new-num-nodes (count (graph/nodes new-graph-struct))
        new-node-infos (merge node-infos (node-info-for-mid-edge-nodes new-graph-struct))
        new-edge-infos (merge edge-infos (edge-info-for-mid-edge-edges new-graph-struct))]
    (GraphProps. new-num-nodes shelters new-node-infos new-edge-infos)))

(defn make-dense
  [{remaining-people :remaining-people :as graph-desc}]

  (let [new-graph-struct (gutils/dense-graph (:structure graph-desc))
        new-graph-props (props-for-dense graph-desc new-graph-struct)]
    (DenseGraphDescription. new-graph-struct new-graph-props remaining-people true)))