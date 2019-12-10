(ns introai.assignment2.agents.agent
  (:gen-class)
  (:require
    [introai.assignment2.agents.human :refer [interactive]]
    [introai.assignment2.agents.greedy :refer [greedy]]
    [introai.assignment2.agents.mini-max :refer [mini-max]]
    [introai.assignment2.agents.game-funcs :refer [MAX_EXPAND-LIMIT]]
    [introai.assignment2.game-state :as gs]))

(defn AGENT_PROPS [alg-name]
  ({
    "user"         [
                    interactive
                    ]

    "greedy"       [
                    greedy
                    ]

    "minmax-basic" [
                    mini-max
                    ]
    } alg-name))

(defrecord Agent [choose-op name])

(defn gen-agent [{alg-name :alg-name L :L T :T} agent-name]
  (let [properties (zipmap [:choose-op] (AGENT_PROPS alg-name))]
    (map->Agent (assoc properties :name agent-name))))

