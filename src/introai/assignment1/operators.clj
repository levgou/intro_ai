(ns introai.assignment1.operators
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]))

(defn term [state]
  (assoc state :terminated true
               :carrying 0))

(defn pick-up-people [graph-desc state]
  (assoc state
    :carrying (+ (:carrying state)
                 (gd/people-num graph-desc state))
    :remaining-people (gs/rem-people state)))

(defn put-people-shelter [graph-desc state]
  (if-not (gd/shelter? graph-desc state)
    state
    (assoc (update state :saved + (:carrying state)) :carrying 0)))

(defn edge [graph-desc edge state]
  (let [new-state
        (-> state
            ((partial gs/progress-time edge))
            ((partial gs/traverse-edge edge)))]

    (if (gd/time-over? graph-desc new-state)
      (term new-state)
      (-> new-state
          ((partial put-people-shelter graph-desc))
          ((partial pick-up-people graph-desc))))))

(defn partial-edge [graph-desc state dest]
  (partial edge
           graph-desc
           (gd/find-edge graph-desc (:agent-node state) (Integer. dest))))