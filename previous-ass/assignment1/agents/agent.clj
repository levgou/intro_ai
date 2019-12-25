(ns introai.assignment1.agents.agent
  (:gen-class)
  (:require
    [introai.assignment1.agents.human :refer [interactive]]
    [introai.assignment1.agents.greedy :refer [greedy]]
    [introai.assignment1.agents.greedy-search :refer [greedy-tree-search]]
    [introai.assignment1.agents.a-star :refer [a-star-search gen-rt-a-star-search]]
    [introai.assignment1.agents.game-funcs :refer [MAX_EXPAND-LIMIT]]
    ))


(def id-graph identity)

(defn gen-penalize [T L]
  (fn [graph-desc] (update-in graph-desc [:props :time-modifier] - (* T L))))

(defn AGENT_PROPS [alg-name T L]
  ({
    "user"          [
                     interactive
                     (gen-penalize T 1)
                     id-graph
                     ]

    "greedy"        [
                     greedy
                     (gen-penalize T 1)
                     id-graph
                     ]

    "greedy-search" [
                     greedy-tree-search
                     (gen-penalize 0 0)
                     id-graph
                     ]

    "a-star"        [
                     a-star-search
                     (gen-penalize T MAX_EXPAND-LIMIT)
                     id-graph
                     ]

    "rt-a-star"     [
                     (gen-rt-a-star-search L)
                     (gen-penalize T L)
                     id-graph
                     ]

    "vandal"        [
                     greedy
                     (gen-penalize T 1)
                     id-graph
                     ]
    } alg-name))

(defrecord Agent [choose-op time-penalizer alter-graph])

(defn gen-agent [{alg-name :alg-name L :L T :T}]
  (let [properties (zipmap [:choose-op :time-penalizer :alter-graph] (AGENT_PROPS alg-name T L))]
    (map->Agent properties)))

