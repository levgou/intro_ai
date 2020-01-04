(ns introai.assignment2.run-game
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.assignment2.read-graph :as rg]
            [introai.assignment2.agents.game-funcs :refer [all-agents-term? agent-term?]]
            [introai.assignment2.agents.agent :refer [gen-agent]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            [nano-id.core :refer [nano-id]]
            [clojure.tools.cli :refer [parse-opts]]
            ))

(def CMD-OPTS
  [
   ["-a" "--alg ALG-NAME" "One of: [minmax, semi-coop, coop]"
    :id :alg-name]
   ["-c" "--cutoff-depth int POSITIVE-INTEGER" "depth of game tree allowed"
    :parse-fn #(Integer/parseInt %)
    :default 9
    :id :cutoff-depth]
   ["-f" "--file-name FILE-NAME" "path to runtime file" :id :file-name]
   ["-h" "--help"]
   ])

(defn opt-parser [args]
  (:options (parse-opts args CMD-OPTS)))

(defn perform-next-op
  [{choose-op :choose-op :as agent}
   graph-desc di-state agent-order]
  (let [op (choose-op graph-desc di-state agent-order)]
    (assoc-in (op graph-desc di-state agent) [1 :id] (nano-id 7))))

(defn agent-alter-world [graph-desc di-state agent-order]
  (let [agent (first agent-order)]
    (if-not (agent-term? di-state agent)
      (perform-next-op agent graph-desc di-state agent-order)
      [graph-desc di-state])))

(defn main-loop
  [graph-desc di-state agents]
  (log/two-state graph-desc di-state)

  (loop [cur-graph-desc graph-desc
         cur-di-state di-state]

    (if (all-agents-term? cur-di-state agents)
      [cur-graph-desc cur-di-state]

      (let [[graph-desc1 di-state1] (agent-alter-world cur-graph-desc cur-di-state agents)
            [graph-desc2 di-state2] (agent-alter-world graph-desc1 di-state1 (reverse agents))]

        (log/two-state graph-desc2 di-state2)
        (recur graph-desc2 (gs/progress-time di-state2 1))))))

(defn run [graph-desc di-state agents]
  (log/info "START")
  (let [[final-graph-desc final-di-state]
        (main-loop graph-desc di-state agents)]

    (log/end final-graph-desc final-di-state)
    ;(log/exe-summary (gen-summary final-state stats final-graph-desc))
    ))

(defn run-from-opts [opts]
  (let [graph-desc (rg/read-graph-from-file (:file-name opts))]
    (let [agent1 (gen-agent opts "Alice")
          agent2 (gen-agent opts "Bob")]
      (run graph-desc (gs/initial-di-state) [agent1 agent2]))))
