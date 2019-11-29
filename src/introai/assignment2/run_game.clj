(ns introai.assignment2.run-game
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.assignment2.graph-description :as gd]
            [introai.assignment2.read-graph :as rg]
            [introai.assignment2.agents.agent :refer [gen-agent]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            [clojure.tools.cli :refer [parse-opts]]
            ))

(def CMD-OPTS
  [
   ["-a" "--alg ALG-NAME" "One of: [greedy, greedy-search, a-star, rt-a-star]"
    :id :alg-name]
   ["-T" "--T T-VAL" :default 0 :id :T :parse-fn #(Float. %)]
   ["-L" "--L L-VAL" :default 0 :id :L :parse-fn #(Integer. %)]
   ["-f" "--file-name FILE-NAME" "path to runtime file" :id :file-name]
   ["-h" "--help"]
   ])

(defn opt-parser [args]
  (:options (parse-opts args CMD-OPTS)))

(defn gen-summary [final-state rt-stats final-graph-desc]
  {
   :score               (:score final-state)
   :num-expands         (:num-expands rt-stats)
   :num-edges-traversed (:iteration rt-stats)
   :time                (:time final-state)
   :saved               (:saved final-state)
   :remaining-people    (:remaining-people final-state)
   :final-node          (:agent-node final-state)
   :time-penalties      (-> final-graph-desc :props :time-modifier)
   })

(defrecord RtStats [iteration num-expands])
(defn iter+ [rt-stats] (update rt-stats :iteration inc))
(defn expand-counter [rt-stats] #(update rt-stats :num-expands + %))

(defrecord OpInProgress [agent op resolve-time])
(defn resolvable? [op-in-progress cur-time] (>= cur-time (:resolve-time op-in-progress)))

(defn gen-deferred-op
  [{choose-op :choose-op time-penalizer :time-penalizer :as agent}
   graph-desc cur-time]
  (let [time-updated-gdesc (time-penalizer graph-desc)]
    (let [op (choose-op time-updated-gdesc (:state agent) cur-time)]
      (OpInProgress. agent op (:resolve-time op)))))

(defn agent-term? [agent] (gs/term? (:state agent)))
(defn agent-active? [agent] (not (agent-term? agent)))

(defn update-agent-state
  [cur-time [graph-desc updated-agents] {op :op agent :agent}]
  (let [[new-graph-desc new-state] (op graph-desc (:state agent) cur-time)]
    [new-graph-desc
     (conj updated-agents (assoc agent :state new-state))]))

(defn gen-ops-in-progress
  [agents graph-desc cur-time]
  (map #(gen-deferred-op % graph-desc cur-time)
       agents))

(defn resolve-ops [graph-desc ops cur-time]
  (reduce (partial update-agent-state cur-time) [graph-desc '()] ops))

(defn resolve-ready-ops [graph-desc ops cur-time]
  (let [resolvable-ops (filter #(resolvable? % cur-time) ops)]
    (resolve-ops graph-desc resolvable-ops cur-time)))

(defn main-loop
  [graph-desc agents]

  (let [initial-ops (gen-ops-in-progress agents graph-desc 0)]
    (loop [cur-graph-desc graph-desc
           ops-in-progress initial-ops
           cur-time 0]

      (let [[updated-graph-desc agents-new-state] (resolve-ready-ops cur-graph-desc ops-in-progress cur-time)
            not-ready-ops (remove #(resolvable? % cur-time) ops-in-progress)]

        (if (every? empty? [(filter agent-active? agents-new-state) not-ready-ops])
          [updated-graph-desc agents-new-state]

          (let [new-ops (gen-ops-in-progress agents-new-state updated-graph-desc cur-time)]
            (let [remaining-ops (concat not-ready-ops new-ops)]

              (log/iteration cur-time agents-new-state remaining-ops updated-graph-desc)
              (recur updated-graph-desc
                     remaining-ops
                     (inc cur-time)))))))))

(defn run [graph-desc agents]
  (log/info "START")
  (let [[final-graph-desc final-state-agents]
        (main-loop graph-desc agents)]

    (log/end final-graph-desc final-state-agents)
    ;(log/exe-summary (gen-summary final-state stats final-graph-desc))
    ))

(defn run-from-opts [opts]
  (let [graph-desc (rg/read-graph-from-file (:file-name opts))]
    (let [agent1 (gen-agent opts (gs/initial-state) "Bob")
          agent2 (gen-agent opts (gs/initial-state) "Alice")]
      (run graph-desc [agent1 agent2]))))
