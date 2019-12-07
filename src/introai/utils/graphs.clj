(ns introai.utils.graphs
  (:gen-class)
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.attr :as g-attr]
            [clojure.core.strint :refer [<<]]
            [introai.utils.collections :refer [pairwise-collection]]
            ))


(defn edge-from-src-target-node [game-graph src-node target-node]
  {
   :weight (graph/weight game-graph src-node target-node)
   :src    src-node
   :dest   target-node
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

(defn unique-edges [g]
  (into #{} (map sort (graph/edges g))))

(defn edge-map [g [src dest]]
  {
   :src    src
   :dest   dest
   :weight (graph/weight g src dest)
   })

(defn mid-vertex-name [src dest mid-id]
  (<< "~{src}-~{dest}-~{mid-id}"))

(defn mid-vertices [{src :src dest :dest weight :weight}]
  (map #(mid-vertex-name src dest %) (range 1 weight)))

(defn new-path [{src :src dest :dest :as edge}]
  (concat [src] (mid-vertices edge) [dest]))

(defn unique-weighted-edges [g]
  (let [edges (unique-edges g)]
    (map #(edge-map g %) edges)))

(defn edges-to-add-for-edge [edge]
  (let [p (new-path edge)]
    (pairwise-collection p)))

(defn new-nodes [g new-edges]
  (let [old-nodes (graph/nodes g)]
    (remove #(contains? old-nodes %) (into #{} (flatten new-edges)))))

(defn edges-to-add [g]
  (let [new-edges (apply concat (map edges-to-add-for-edge (unique-weighted-edges g)))]
    new-edges))

(defn dense-graph [g]
  (let [new-edges (edges-to-add g)]
    (let [nodes (new-nodes g new-edges)]
      (println new-edges)
      (println nodes)
      (-> g
          (#(apply graph/add-edges % new-edges))
          (g-attr/add-attr-to-nodes :mid-edge true nodes)))))