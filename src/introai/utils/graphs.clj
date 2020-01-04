(ns introai.utils.graphs
  (:gen-class)
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.attr :as g-attr]
            [clojure.core.strint :refer [<<]]
            [introai.utils.collections :refer [pairwise-collection]]
            [introai.utils.log :as log]
            [introai.assignment3.game-state :as gs]
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

(defn shortest-path-dist
  "Returns shortest path and its length"
  [g src dest]
  ((juxt identity #(dec (count %))) (alg/shortest-path g src dest)))

(defn shortest-path-len
  [g src dest]
  (second (shortest-path-dist g src dest)))

(defn node-mid-edge?
  ([graph-struct node]
   (true? (g-attr/attr graph-struct node :mid-edge)))

  ([graph-desc di-state agent]
   (node-mid-edge? (:structure graph-desc) (gs/state-piece-of di-state agent :agent-node))))

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
  (let [edges (graph/edges g)]
    (map #(edge-map g %) edges)))

(defn edges-to-add-for-edge [edge]
  (let [p (new-path edge)]
    (pairwise-collection p)))

(defn new-nodes-only [g new-edges]
  (let [old-nodes (graph/nodes g)]
    (remove #(contains? old-nodes %) (into #{} (flatten new-edges)))))

(defn break-down-edges [g]
  (let [new-edges (apply concat (map edges-to-add-for-edge (unique-weighted-edges g)))]
    new-edges))

(defn dense-graph [g]
  (let [new-edges (break-down-edges g)]
    (let [new-nodes (new-nodes-only g new-edges)]
      (-> (apply graph/digraph new-edges)
          (g-attr/add-attr-to-nodes :mid-edge true new-nodes)))))

(defn dense? [graph-desc]
  (:dense graph-desc))

(defn weight [graph-desc src-int dest-int]
  (if (dense? graph-desc) 1 (graph/weight (:structure graph-desc) src-int dest-int)))

(defn mid-nodes [graph-struct]
  (filter (partial node-mid-edge? graph-struct) (graph/nodes graph-struct)))

(defn mid-node-in-edge [graph-struct edge-vec]
  (any?
    (map (partial node-mid-edge? graph-struct) edge-vec)))

(defn mid-edges [graph-struct]
  (filter (partial mid-node-in-edge graph-struct) (graph/edges graph-struct)))