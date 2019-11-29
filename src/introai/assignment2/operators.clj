(ns introai.assignment2.operators
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.assignment2.graph-description :as gd]
            [loom.graph :as graph]
            [nano-id.core :refer [nano-id]]
            [introai.utils.collections :refer [in?]]
            [introai.utils.enums :as E]
            [introai.utils.log :as log]))

(defn update-dead-count [{dead-map :dead :as state} graph-desc]
  (-> dead-map
      (update E/DIED-IN-CITY + (gd/all-people graph-desc))
      (update E/DIED-WITH-AGENT + (:carrying state))))

(defn calc-final-g [state]
  (+
    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))))

(defn pick-up-people [[graph-desc state]]
  (let [ppl-at-agent-node (gd/people-num graph-desc state)]
    [
     (gs/rem-people graph-desc state)
     (assoc state
       :carrying (+ (:carrying state) ppl-at-agent-node)
       :id (nano-id 10))
     ]))

(defn put-people-shelter [[graph-desc state]]
  (if-not (gd/shelter? graph-desc state)
    [graph-desc state]
    [graph-desc (assoc (update state :saved + (:carrying state)) :carrying 0)]))

(defn term-type-of [graph-desc state]
  (if (gd/shelter? graph-desc state) E/TERMINATED-SAFELY E/TERMINATED-UNSAFELY))

(defn calc-final-score [state]
  (assoc state :score (calc-final-g state)))

(defrecord Term [resolve-time src dest op-type]
  clojure.lang.IFn

  (invoke [this graph-desc state cur-time]
    (let [final-state (calc-final-score
                        (assoc state :terminated (term-type-of graph-desc state)
                                     :dead (update-dead-count state graph-desc)
                                     :carrying 0
                                     :id (nano-id 10)))]
      [graph-desc final-state])))

(defn make-term [vertex]
  (Term. 0 vertex vertex E/T_TERM))

(defrecord Edge [resolve-time src dest op-type]
  clojure.lang.IFn

  (invoke [this graph-desc state cur-time]
    (log/debug "Got to: " dest " at: " cur-time)
    (let [new-state (gs/traverse-edge dest state)]
      (if-not (gd/time-over? graph-desc state cur-time)
        (pick-up-people (put-people-shelter [graph-desc new-state]))
        ((make-term dest) graph-desc new-state cur-time)))))

(defn make-edge [graph-desc state dest cur-time]
  (let [dest-int (Integer. dest)
        src-int (:agent-node state)
        g (:structure graph-desc)]

    (Edge. (+ cur-time (graph/weight g src-int dest-int))
           src-int
           dest-int
           E/T_EDGE)))

(defrecord Oracle [knows-all state-transitions]
  clojure.lang.IFn
  (invoke [this state] (state-transitions state)))
