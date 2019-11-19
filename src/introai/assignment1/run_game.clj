(ns introai.assignment1.run-game
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]
            [introai.assignment1.read-graph :as rg]
            [introai.assignment1.agents.human :refer [interactive]]
            [introai.assignment1.agents.greedy :refer [greedy]]
            [introai.assignment1.agents.greedy-search :refer [greedy-tree-search]]
            [introai.assignment1.agents.a-star :refer [a-star-search gen-rt-a-star-search]]
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

(defn gen-summary [final-state score edges-traversed total-expands]
  {
   :score            score
   :num-expands      total-expands
   :edges-traversed  edges-traversed
   :time             (:time final-state)
   :saved            (:saved final-state)
   :remaining-people (:remaining-people final-state)
   :final-node       (:agent-node final-state)
   }
  )

(defn gen-penalize [T L]
  (fn [graph-desc] (update-in graph-desc [:props :time-modifier] - (* T L))))

(defn choose-alg-time-penalize [{alg-name :alg-name L :L T :T}]
  (case alg-name
    "greedy" [greedy (gen-penalize T 1)]
    "greedy-search" [greedy-tree-search (gen-penalize 0 0)]
    "a-star" [a-star-search (gen-penalize 0 0)]
    "rt-a-star" [(gen-rt-a-star-search L) (gen-penalize T L)]
    ))


(defrecord RtStats [iteration num-expands])
(defn iter+ [rt-stats] (update rt-stats :iteration inc))
(defn expand-counter [rt-stats] #(update rt-stats :num-expands + %))

(defn main-loop [graph-desc init-state choose-op time-penalizer]

  (let [init-stats (RtStats. 0 0)]
    (loop [state init-state
           stats init-stats
           cur-graph-desc graph-desc
           prev-op {}]

      (log/info (<< "(~{iteration})> ~{(into {} state)}"))
      (if (gs/term? init-state)
        [init-state stats graph-desc]

        (if (:knows-all prev-op)
          (recur (prev-op state) (iter+ stats) cur-graph-desc prev-op)

          (let [time-updated-gdesc (time-penalizer cur-graph-desc)]
            (let [[op stats] (choose-op time-updated-gdesc state (expand-counter stats))]
              (recur (op state) (iter+ stats) time-updated-gdesc op))))


        ;(let [time-updated-gdesc (time-penalizer cur-graph-desc)]
        ;
        ;  (let [[op cur-score num-expand]
        ;        (if (:knows-all prev-op) [prev-op score 0] (choose-op time-updated-gdesc state))]
        ;
        ;    (recur
        ;      (op state)
        ;      (iter+ stats)
        ;      time-updated-gdesc
        ;      op)))
        ))))

(defn run [graph-desc alg time-penalize]
  (log/info "START")
  (let [[final-state score edges-traversed total-expands final-graph-desc]
        (main-loop
          graph-desc
          (gs/initial-state (gd/people-map graph-desc))
          alg
          time-penalize)]

    (log/info "END:" (into {} final-state))
    (log/exe-summary (gen-summary final-state score edges-traversed total-expands))))

(defn run-from-opts [opts]
  (let [graph-desc (rg/read-graph-from-file (:file-name opts))
        [alg time-penalize] (choose-alg-time-penalize opts)]
    (run graph-desc alg time-penalize)))