(ns introai.assignment1.operators
  (:gen-class)
  (:require [introai.assignment1.game-state :as gs]
            [introai.assignment1.graph-description :as gd]
            [loom.graph :as graph]
            [nano-id.core :refer [nano-id]]
            [introai.utils.collections :refer [in?]]
            [introai.utils.enums :as E]
            ))

(defn update-dead-count [{dead-map :dead :as state}]
  (-> dead-map
      (update E/DIED-IN-CITY + (gd/all-people state))
      (update E/DIED-WITH-AGENT + (:carrying state))))

(defn calc-final-g [state]
  (+
    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))))

(defn term [graph-desc state]
  (let [final-state
        (assoc state :terminated (if (gd/shelter? graph-desc state) E/TERMINATED-SAFELY E/TERMINATED-UNSAFELY)
                     :dead (update-dead-count state)
                     :carrying 0
                     :id (nano-id 10))]
    (assoc final-state :score (calc-final-g final-state))))

(defn partial-term [graph-desc]
  (partial term graph-desc))

(defn pick-up-people [graph-desc state]
  (assoc state
    :carrying (+ (:carrying state)
                 (gd/people-num graph-desc state))
    :remaining-people (gs/rem-people state)
    :id (nano-id 10)))

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
      (term graph-desc new-state)
      (-> new-state
          ((partial put-people-shelter graph-desc))
          ((partial pick-up-people graph-desc))))))

(defn partial-edge [graph-desc state dest]
  (let [dest-int (Integer. dest)
        src-int (:agent-node state)
        g (:structure graph-desc)]

    (partial edge
             graph-desc
             {:src src-int :dest dest-int :weight (graph/weight g src-int dest-int)})))

(defrecord Oracle [knows-all state-transitions]
  clojure.lang.IFn
  (invoke [this state] (state-transitions state)))
