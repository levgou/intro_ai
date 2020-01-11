(ns introai.assignment3.run-game
  (:gen-class)
  (:require [introai.assignment3.game-state :as gs]
            [introai.assignment3.agents.game-funcs :refer [all-agents-term? agent-term?]]
            [introai.assignment3.agents.agent :refer [gen-agent]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            [nano-id.core :refer [nano-id]]
            [clojure.tools.cli :refer [parse-opts]]
            [introai.assignment3.read-bayes :refer [read-graph-from-file]]
            [introai.assignment3.bayes-network :refer [gen-b-net-t-0]]
            [introai.assignment3.main-loop :refer [main-loop]]
            ))

(def CMD-OPTS
  [
   ["-f" "--file-name FILE-NAME" "path to runtime file" :id :file-name]
   ["-h" "--help"]
   ])

(defn opt-parser [args]
  (:options (parse-opts args CMD-OPTS)))

;(defn perform-next-op
;  [{choose-op :choose-op :as agent}
;   graph-desc di-state agent-order]
;  (let [op (choose-op graph-desc di-state agent-order)]
;    (assoc-in (op graph-desc di-state agent) [1 :id] (nano-id 7))))
;
;(defn agent-alter-world [graph-desc di-state agent-order]
;  (let [agent (first agent-order)]
;    (if-not (agent-term? di-state agent)
;      (perform-next-op agent graph-desc di-state agent-order)
;      [graph-desc di-state])))
;
;(defn main-loop
;  [graph-desc di-state agents]
;  (log/two-state graph-desc di-state)
;
;  (loop [cur-graph-desc graph-desc
;         cur-di-state di-state]
;
;    (if (all-agents-term? cur-di-state agents)
;      [cur-graph-desc cur-di-state]
;
;      (let [[graph-desc1 di-state1] (agent-alter-world cur-graph-desc cur-di-state agents)
;            [graph-desc2 di-state2] (agent-alter-world graph-desc1 di-state1 (reverse agents))]
;
;        (log/two-state graph-desc2 di-state2)
;        (recur graph-desc2 (gs/progress-time di-state2 1))))))
;
;(defn run [graph-desc di-state agents]
;  (log/info "START")
;  (let [[final-graph-desc final-di-state]
;        (main-loop graph-desc di-state agents)]
;
;    (log/end final-graph-desc final-di-state)
;    ;(log/exe-summary (gen-summary final-state stats final-graph-desc))
;    ))

(defn run-from-opts [opts]
  (let [graph-desc (read-graph-from-file (:file-name opts))
        bayes (gen-b-net-t-0 graph-desc (-> graph-desc :props :persist))]
    (main-loop graph-desc bayes)))
