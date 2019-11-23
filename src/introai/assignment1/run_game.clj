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

(defn main-loop [graph-desc init-state agent]
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