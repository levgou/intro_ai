(ns introai.assignment1.agents.tree-search
  (:gen-class)
  (:require [loom.graph :as graph]
            [introai.utils.log :as log]
            [clojure.core.strint :refer [<<]]
            [loom.alg :as alg]
            ))

(defrecord StateNode [state op g h f])

(defn make-node
  [{state :state op :op g :g h :h}]
  (StateNode. state op g h (+ g h)))

(defn first-node [state]
  (make-node {:state state :op nil :g 0 :h 0}))

(defn first-op [state-tree src-node target-node]
  (let [shortest-path (alg/shortest-path state-tree src-node target-node)]
    (-> shortest-path second :op)))

(defn state-expand
  [expand-f fringe state-tree node]
  (log/debug "Expanding: " (log/state-node node))
  (let [next-state-nodes (map make-node (expand-f (:state node)))]
    [
     (into fringe next-state-nodes)
     (apply graph/add-edges
            (apply graph/add-nodes state-tree next-state-nodes)
            (map #(vector node %) next-state-nodes))
     ]))

(defn traverse-tree
  [fringe state-tree goal? expand]
  (let [src-node (peek fringe)]

    (loop [[cur-fringe states] [fringe state-tree]]

      (let [min-node (peek cur-fringe) others (pop cur-fringe)]
        (log/debug "Min Node: " (log/state-node min-node))

        (if (nil? min-node)
          [nil states]

          (if (goal? min-node)
            (do (log/debug "Goal: " (log/state-node min-node))
                [(first-op states src-node min-node) states])

            (recur (state-expand expand others states min-node))))))))

(defn init-fringe [fringe init-state]
  (-> init-state first-node (#(conj fringe %))))

(defn init-tree [state-tree initial-fringe]
  (graph/add-nodes state-tree (first initial-fringe)))

(defn tree-search
  [init-state fringe goal? state-tree expand]
  (let [initial-fringe (init-fringe fringe init-state)
        initial-tree (init-tree state-tree initial-fringe)]

    (let [[op state-tree]
          (traverse-tree initial-fringe initial-tree goal? expand)]
      op)))