(ns introai.assignment2.agents.agent
  (:gen-class)
  (:require
    [introai.assignment2.agents.human :refer [interactive]]
    [introai.assignment2.agents.greedy :refer [greedy]]
    [introai.assignment2.agents.mini-max :refer [mini-max]]
    [introai.assignment2.agents.game-funcs :refer [MAX_EXPAND-LIMIT]]
    [introai.assignment2.game-state :as gs]))


(defn gen-penalize [T L]
  (fn [di-state agent]
    (let [cur-mod (gs/state-piece-of di-state agent :time-modifier)]
      (gs/assoc-in-agent di-state agent :time-modifier
                         (- cur-mod (* T L))))))

(defn AGENT_PROPS [alg-name T L]
  ({
    "user"         [
                    interactive
                    (gen-penalize T 1)
                    ]

    "greedy"       [
                    greedy
                    (gen-penalize T 1)
                    ]

    "minmax-basic" [
                    mini-max
                    (gen-penalize 0 0)                      ; todo ????????????///
                    ]
    } alg-name))

(defrecord Agent [choose-op time-penalizer name])

(defn gen-agent [{alg-name :alg-name L :L T :T} agent-name]
  (let [properties (zipmap [:choose-op :time-penalizer] (AGENT_PROPS alg-name T L))]
    (map->Agent (assoc properties :name agent-name))))

