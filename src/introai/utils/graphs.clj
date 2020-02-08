(ns introai.utils.graphs
  (:gen-class)
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.attr :as g-attr]
            [clojure.core.strint :refer [<<]]
            [introai.utils.collections :refer [pairwise-collection]]
            [introai.utils.const :as E]))


(defn same-edge [src dest edge]
  (= (sort [src dest]) (sort [(:start edge) (:end edge)])))

(defn edge-names-at-node [edge-map node]
  (->> edge-map
       (filter #(or (= node (-> % second :start)) (= node (-> % second :end))))
       (map first)))


(defn relevant-edge [g-desc state dest]
  (let [agent-node (:agent-node state)]
    (->> g-desc
         :props
         :edges
         vals
         (filter #(same-edge agent-node dest %))
         first)))


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

(defn edge-unblocked? [graph-desc state dest]
  (= E/BLOCKED-FALSE (:blocked (relevant-edge graph-desc state dest))))

(defn state-unblocked-successors [graph-desc state]
  (let [succ (state-successors graph-desc state)]
    (filter
      #(edge-unblocked? graph-desc state %)
      succ)))

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
