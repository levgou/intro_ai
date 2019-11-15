(ns introai.assignment1.agents.tree-search
  (:gen-class)
  (:require [loom.graph :as graph]))

(defrecord StateNode [state g h f])

(defn make-node
  [{state :state g :g h :h}]
  (StateNode. state g h (+ g h)))

(defn state-expand
  [expand-f fringe state-tree node]
  (let [next-state-nodes (map make-node (expand-f (:state node)))]
    [
     (apply conj fringe next-state-nodes)
     (apply graph/add-edges
            (apply graph/add-nodes state-tree next-state-nodes)
            (map #([node %]) next-state-nodes))
     ]))

(defn traverse-tree
  [fringe state-tree goal? expand]
  (loop [[[min-node & others] states] [fringe state-tree]]
    (if (nil? min-node)
      nil [states]
      (if (goal? min-node)
        [min-node states]
        (recur (state-expand expand others states min-node))))))

(defn init-fringe [fringe init-state]
  (-> init-state make-node (#(conj fringe %))))

(defn init-tree [state-tree initial-fringe]
  (graph/add-nodes state-tree (first initial-fringe)))

(defn tree-search
  [init-state fringe goal? expand state-tree]
  (let [initial-fringe (init-fringe fringe init-state)
        initial-tree (init-tree state-tree init-state)]
    (traverse-tree initial-fringe initial-tree goal? expand)))