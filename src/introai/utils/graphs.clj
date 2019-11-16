(ns introai.utils.graphs
  (:gen-class)
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            ))


(defn edge-from-src-target-node [game-graph src-node target-node]
  {
   :weight (graph/weight game-graph src-node target-node)
   :src src-node
   :dest target-node
   })

(defn edge-from-state-target-node [game-graph state target-node]
  (let [src-node (:agent-node state)]
    (edge-from-src-target-node game-graph src-node target-node)))

(defn successors [graph-desc node]
  (graph/successors (:structure graph-desc) node))

(defn state-successors [graph-desc state]
  (successors graph-desc (:agent-node state)))

(defn predecessors [graph-desc node]
  (graph/predecessors (:structure graph-desc) node))

(defn add-nodes [graph-desc & nodes]
  (apply graph/add-nodes (:structure graph-desc) nodes))

(defn add-edges [graph-desc & edges]
  (apply graph/add-nodes (:structure graph-desc) edges))

(defn dijkstra-dist [g src dest]
  (second (alg/dijkstra-path-dist g src dest)))