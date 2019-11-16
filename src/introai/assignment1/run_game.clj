(ns introai.assignment1.run-game
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]
            [introai.assignment1.read-graph :as rg]
            [introai.assignment1.agents.human :refer [interactive]]
            [introai.assignment1.agents.greedy :refer [greedy]]
            [introai.assignment1.agents.greedy-search :refer [greedy-tree-search]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            ))
(def SUMMARY-FIELDS [:score :num-expands :time :saved :remaining-people :final-node :edges-traversed])

(defn gen-summary [final-state]
  {
   :time             (:time final-state)
   :saved            (:saved final-state)
   :remaining-people (:remaining-people final-state)
   :final-node       (:final-node final-state)
   }
  )

(defn main-loop [graph-desc init-state choose-op]
  (loop [state init-state iteration 0]
    (log/info (<< "(~{iteration})> ~{(into {} state)}"))
    (if (gs/term? state)
      state
      (recur
        ((choose-op graph-desc state) state)
        (inc iteration)))))

(defn run [graph-desc]
  (log/info "START")
  (let [final-state (main-loop
                      graph-desc
                      (gs/initial-state (gd/people-map graph-desc))
                      greedy-tree-search)]
    (log/info "END:" (into {} final-state))))

(defn run-from-file [file-abs]
  (let [graph-desc (rg/read-graph-from-file file-abs)]
    (run graph-desc)))