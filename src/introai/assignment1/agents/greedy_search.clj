(ns introai.assignment1.agents.greedy-search
  (:gen-class)
  (:require [loom.graph :as graph]
            [introai.assignment1.agents.tree-search :refer [tree-search]]
            [introai.assignment1.agents.game-funcs :refer [goal? gen-state-expander]]))

(defn greedy-key [node] (:h node))

(defn make-greedy-fringe []
  (sorted-set-by greedy-key))

(defn greedy-tree-search
  [graph-desc state]
  (tree-search
    state
    (make-greedy-fringe)
    goal?
    graph/digraph
    (gen-state-expander graph-desc)))
