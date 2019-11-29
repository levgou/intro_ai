(ns introai.assignment1.run-game
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]
            [introai.assignment1.read-graph :as rg]
            [introai.assignment1.agents.agent :refer [gen-agent]]
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
    (let [[op resolve-time] (choose-op time-updated-gdesc (:state agent) cur-time)]
      (OpInProgress. agent op resolve-time))))

(defn main-loop1 [graph-desc init-state agent]
  (let [init-stats (RtStats. 0 0)
        {time-penalizer :time-penalizer choose-op :choose-op} agent]

    (loop [state init-state
           stats init-stats
           cur-graph-desc graph-desc
           prev-op {}]

      (log/info (<< "(~{(:iteration stats)})> ~{(into {} state)}"))
      (if (gs/term? state)
        [state stats cur-graph-desc]

        (if (:knows-all prev-op)
          (recur (prev-op state) (iter+ stats) cur-graph-desc prev-op)

          (let [time-updated-gdesc (time-penalizer cur-graph-desc)]
            (let [[op stats] (choose-op time-updated-gdesc state (expand-counter stats))]
              (recur (op state) (iter+ stats) time-updated-gdesc op))))))))

(defn agent-term? [agent] (gs/term? (:state agent)))
(defn agent-active? [agent] (not (agent-term? agent)))

(defn update-agent-state
  [[updated-agents graph-desc] {op :op agent :agent}]
  (let [[new-state new-graph-desc] (op (:state agent) graph-desc)]
    [(conj updated-agents (assoc agent :state new-state))
     new-graph-desc]))

(defn gen-ops-in-progress
  [agents graph-desc cur-time]
  (map #(gen-deferred-op % graph-desc cur-time)
       agents))

(defn main-loop [graph-desc agents]

  (loop [cur-graph-desc graph-desc
         idle-agents agents
         ops-in-progress []
         cur-time 0]

    (log/iteration cur-time idle-agents ops-in-progress)

    (if (every? empty? [(filter agent-active? idle-agents) ops-in-progress])
      [cur-graph-desc idle-agents]

      (let [resolvable-ops (filter #(resolvable? % cur-time) ops-in-progress)]
        (let [[agents-new-state updated-graph-desc]
              (reduce update-agent-state [cur-graph-desc '()] resolvable-ops)]

          (recur [updated-graph-desc
                  agents-new-state
                  (gen-ops-in-progress idle-agents updated-graph-desc cur-time)
                  (inc cur-time)]))))))


(defn run [graph-desc agent]
  (log/info "START")
  (let [[final-state stats final-graph-desc]
        (main-loop
          graph-desc
          (gs/initial-state (gd/people-map graph-desc))
          agent)]

    (log/info "END:" (into {} final-state))
    (log/exe-summary (gen-summary final-state stats final-graph-desc))))

(defn run-from-opts [opts]
  (let [graph-desc (rg/read-graph-from-file (:file-name opts))
        agent (gen-agent opts)]
    (run graph-desc agent)))