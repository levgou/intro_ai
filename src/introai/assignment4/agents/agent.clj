(ns introai.assignment4.agents.agent
  (:gen-class)
  (:require
    [introai.assignment4.agents.human :refer [interactive]]
    [introai.assignment4.agents.greedy :refer [greedy]]
    [introai.assignment4.agents.min-max :refer [min-max]]
    [introai.assignment4.agents.max-max :refer [max-max]]
    [introai.assignment4.agents.game-funcs :refer
     [MAX_EXPAND-LIMIT player-max-sort-key player-min-sort-key
      identity-maxifier rev-identity-maxifier coop-maxifier]]
    [introai.assignment4.game-state :as gs]))

(defn AGENT_PROPS [alg-name]
  ({

    "minmax"    [
                 min-max
                 player-max-sort-key
                 player-min-sort-key
                 ]
    "semi-coop" [
                 max-max
                 identity-maxifier
                 rev-identity-maxifier
                 ]
    "coop"      [
                 max-max
                 coop-maxifier
                 coop-maxifier
                 ]
    } alg-name))

(defrecord Agent [choose-op maxifier1 maxifier2 cutoff-depth name]
  Object
  (toString [x] name))
(defmethod print-method Agent [x ^java.io.Writer w] (.write w (str x)))

(defn gen-agent [{alg-name :alg-name cutoff-depth :cutoff-depth} agent-name]
  (let [properties (zipmap [:choose-op :maxifier1 :maxifier2] (AGENT_PROPS alg-name))]
    (map->Agent (assoc properties :name agent-name :cutoff-depth cutoff-depth))))

