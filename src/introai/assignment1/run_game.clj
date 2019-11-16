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
            ))


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

(defn main-loop [graph-desc init-state choose-op]
  (loop [state init-state score 0 iteration 0 total-num-expand 0]
    (log/info (<< "(~{iteration})> ~{(into {} state)}"))
    (if (gs/term? state)
      [state score iteration total-num-expand]
      (let [[op cur-score num-expand] (choose-op graph-desc state)]
        (recur
          (op state)
          cur-score
          (inc iteration)
          (+ total-num-expand num-expand))))))

(defn run [graph-desc]
  (log/info "START")
  (let [[final-state score edges-traversed total-expands]
        (main-loop
          graph-desc
          (gs/initial-state (gd/people-map graph-desc))
          (gen-rt-a-star-search 2))]

    (log/info "END:" (into {} final-state))
    (log/exe-summary (gen-summary final-state score edges-traversed total-expands))))

(defn run-from-file [file-abs]
  (let [graph-desc (rg/read-graph-from-file file-abs)]
    (run graph-desc)))