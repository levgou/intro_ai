(ns introai.assignment1.run-game
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]
            [introai.assignment1.read-graph :as rg]
            [introai.assignment1.agents.human :refer [interactive]]
            [introai.assignment1.agents.greedy :refer [greedy]]
            [clojure.core.strint :refer [<<]]
            [introai.utils.log :as log]
            ))

(defn main-loop [graph-desc init-state choose-op]
  (loop [state init-state iteration 0]
    (log/info (<< "(~{iteration})> ~{(into {} state)}"))
    (if (:terminated state)
      state
      (recur
        ((choose-op graph-desc state) state)
        (inc iteration)))))

(defn run [graph-desc]
  (log/info "START")
  (log/info "END: " (main-loop
                     graph-desc
                     (gs/initial-state (gd/people-map graph-desc))
                     greedy)))

(defn run-from-file [file-abs]
  (let [graph-desc (rg/read-graph-from-file file-abs)]
    (run graph-desc)))