(ns introai.assignment1.agents.a-star
  (:gen-class)
  (:require [loom.graph :as graph]
            [introai.assignment1.agents.tree-search :refer [tree-search]]
            [introai.assignment1.agents.game-funcs :refer [goal? gen-state-expander]]
            [shams.priority-queue :as pq]))

(def MAX_EXPAND-LIMIT 100000)

(defn a-star-key [node] (- (:f node)))

(defn make-a-star-fringe []
  (pq/priority-queue a-star-key))

(defn rt-a-star-search
  [graph-desc state count-expands expand-limit]
  (tree-search
    state
    (make-a-star-fringe)
    goal?
    (graph/digraph)
    (gen-state-expander graph-desc)
    count-expands
    #(<= expand-limit %)))

(defn gen-rt-a-star-search [expand-limit]
  #(rt-a-star-search %1 %2 %3 expand-limit))

(defn a-star-search
  [graph-desc state count-expands]
  (rt-a-star-search graph-desc state count-expands MAX_EXPAND-LIMIT))
