(ns introai.assignment4.agents.human
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [introai.utils.collections :refer [in?]]
            [loom.graph :as graph]
            [introai.assignment4.operators :as op]
            ))


(defn print-choices [state node-choices]
  (let [choice-vec (vec node-choices)
        state-str (into {} state)]
    (println (<< "\nState: ~{state-str} \nNext steps: ~{choice-vec}"))))

(defn user-choice [graph-desc state]
  (let [neighbors
        (map str (graph/successors (:structure graph-desc) (:agent-node state)))]
    (print-choices state neighbors)

    (let [choice (read-line)]
      (if (in? (conj neighbors "e") choice)
        choice
        (user-choice graph-desc state)))))

(defn interactive [graph-desc state cur-time]
  (let [choice (user-choice graph-desc state)]
    (if (= choice "e")
      (op/make-term (:agent-node state))
      (op/make-edge graph-desc state choice cur-time))))
