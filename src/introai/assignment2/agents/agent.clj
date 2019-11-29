(ns introai.assignment2.agents.agent
  (:gen-class)
  (:require
    [introai.assignment2.agents.human :refer [interactive]]
    [introai.assignment2.agents.greedy :refer [greedy]]
    [introai.assignment2.agents.game-funcs :refer [MAX_EXPAND-LIMIT]]
    ))


(defn gen-penalize [T L]
  (fn [graph-desc]
    (update-in graph-desc [:props :time-modifier] - (* T L))))

(defn AGENT_PROPS [alg-name T L]
  ({
    "user"          [
                     interactive
                     (gen-penalize T 1)
                     ]

    "greedy"        [
                     greedy
                     (gen-penalize T 1)
                     ]
    } alg-name))

(defrecord Agent [choose-op time-penalizer state name])

(defn gen-agent [{alg-name :alg-name L :L T :T} init-state agent-name]
  (let [properties (zipmap [:choose-op :time-penalizer] (AGENT_PROPS alg-name T L))]
    (map->Agent (assoc properties :state init-state :name agent-name))))

