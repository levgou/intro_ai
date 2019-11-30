(ns introai.assignment2.operators
  (:gen-class)
  (:require [introai.assignment2.game-state :as gs]
            [introai.assignment2.graph-description :as gd]
            [clojure.core.strint :refer [<<]]
            [loom.graph :as graph]
            [nano-id.core :refer [nano-id]]
            [introai.utils.collections :refer [in?]]
            [introai.utils.enums :as E]
            [introai.utils.log :as log]))

(defn update-dead-count [{dead-map :dead :as state} graph-desc]
  (-> dead-map
      (update E/DIED-IN-CITY + (gd/all-people graph-desc))
      (update E/DIED-WITH-AGENT + (:carrying state))))

;(defn calc-final-g [state]
;  (+
;    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
;    (-> state :dead E/DIED-IN-CITY)
;    (* 2 (-> state :dead E/DIED-WITH-AGENT))
;    (- (* 3 (:saved state)))
;    ))


(defn calc-final-g [state]
  (-
    (:saved state)

    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))
    ))

(defn pick-up-people [graph-desc di-state agent]
  (let [ppl-at-agent-node (gd/people-num graph-desc (gs/state-of di-state agent))
        carrying (gs/state-piece-of di-state agent :carrying)]
    [
     (gs/rem-people graph-desc (gs/state-of di-state agent))
     (-> di-state
         (gs/assoc-in-agent agent :carrying (+ carrying ppl-at-agent-node))
         (gs/assoc-in-agent agent :id (nano-id 10)))
     ]))

(defn update-saved [di-state agent]
  (gs/assoc-in-agent di-state agent :saved
                     (+ (gs/state-piece-of di-state agent :saved)
                        (gs/state-piece-of di-state agent :carrying))))

(defn put-people-shelter [graph-desc di-state agent]
  (if-not (gd/shelter? graph-desc (gs/state-of di-state agent))
    di-state
    (-> di-state (update-saved agent) (gs/assoc-in-agent agent :carrying 0))))

(defn term-type-of [graph-desc state]
  (if (gd/shelter? graph-desc state) E/TERMINATED-SAFELY E/TERMINATED-UNSAFELY))

(defn calc-final-score [di-state agent]
  (gs/assoc-in-agent di-state agent :score (calc-final-g (gs/state-of di-state agent))))

(defrecord Ident [op-type]
  clojure.lang.IFn

  (invoke [this graph-desc di-state agent]
        [graph-desc di-state]))

(defn make-id []
  (Ident. E/T_ID))

(defrecord Term [src dest op-type]
  clojure.lang.IFn

  (invoke [this graph-desc di-state agent]
    (let [agent-state (gs/state-of di-state agent)]
      (let [final-agent-di-state (-> di-state
                                     (gs/assoc-in-agent agent :terminated (term-type-of graph-desc agent-state))
                                     (gs/assoc-in-agent agent :dead (update-dead-count agent-state graph-desc))
                                     (gs/assoc-in-agent agent :carrying 0)
                                     (gs/assoc-in-agent agent :id (nano-id 10))
                                     (calc-final-score agent))]
        [graph-desc final-agent-di-state]))))

(defn make-term [vertex]
  (Term. vertex vertex E/T_TERM))

(defrecord Edge [traversal-time src dest op-type]
  clojure.lang.IFn

  (invoke [this graph-desc di-state agent]
    (let [new-di-state (-> di-state
                           (gs/traverse-edge agent dest)
                           (gs/progress-time traversal-time))]

      (if-not (gd/time-over? graph-desc di-state agent)
        (do (log/debug (<< "[~{(:name agent)}] Got to node [~{dest}] at time [~{(:time new-di-state)}]"))
          (pick-up-people
            graph-desc
            (put-people-shelter graph-desc new-di-state agent)
            agent))

        ((make-term dest) graph-desc new-di-state agent)))))

(defn make-edge [graph-desc state dest]
  (let [dest-int (Integer. dest)
        src-int (:agent-node state)
        g (:structure graph-desc)]

    (Edge. (graph/weight g src-int dest-int)
           src-int
           dest-int
           E/T_EDGE)))

(defrecord Oracle [knows-all state-transitions]
  clojure.lang.IFn
  (invoke [this state] (state-transitions state)))
