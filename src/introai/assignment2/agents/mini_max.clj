(ns introai.assignment2.agents.mini-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term?]]
    [introai.assignment2.game-state :as gs]
    [introai.utils.log :as log]
    [introai.utils.enums :as E]))


(declare min-value)
(declare max-value)
(declare assoc-res-with-min-val)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(def INF ##Inf)
(def -INF (- ##Inf))

(defrecord MiniMaxState [graph-desc di-state agent-order]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MiniMaxState [x ^java.io.Writer w] (.write w (str x)))

(defn minimizers-turn [minimax-state] (:agent-order minimax-state))
(defn maximizers-turn [minimax-state] (repeat 2 (first (:agent-order minimax-state))))

(defrecord MiniMaxNodeProps [alpha beta depth]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MiniMaxNodeProps [x ^java.io.Writer w] (.write w (str x)))

(defn both-agents-terminated [di-state agent-order]
  (log/debug "Bot agents terminated!")
  (and
    (agent-term? di-state (first agent-order))
    (agent-term? di-state (second agent-order))
    ))

(def MAX-DEPTH 13)
(defn cutoff-minimax [minimax-props]
  (log/debug "Depth cutoff!")
  (< MAX-DEPTH (:depth minimax-props)))

(defn term-search? [{di-state :di-state agent-order :agent-order} minimax-props]
  (or
    (both-agents-terminated di-state agent-order)
    (cutoff-minimax minimax-props)))

(defn calc-agent-score [state]
  (-
    (:saved state)

    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))
    ))

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  (into []
        (map #(identity {:op %})
             (gen-next-ops graph-desc di-state op-agent))
        )
  )

(defn stat-eval [{di-state :di-state} agent]
  (calc-agent-score (gs/state-of di-state agent)))

(defn assoc-op-with-res [graph-desc di-state agent map-with-op]
  (let [[new-graph-desc new-di-state] ((:op map-with-op) graph-desc di-state agent)]
    (assoc map-with-op :graph-desc new-graph-desc :di-state new-di-state)))

(defn assoc-ops-with-res
  [{graph-desc :graph-desc di-state :di-state} op-agent ops]
  (into []
        (map #(assoc-op-with-res graph-desc di-state op-agent %) ops)
        )
  )

(defn assoc-res-with-min-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} agent-order minimax-props]
  (let [min-val (min-value (MiniMaxState. graph-desc di-state agent-order) minimax-props)]
    (assoc m :min-val min-val)))

(defn assoc-res-with-max-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} agent-order minimax-props]

  (let [max-val (max-value (MiniMaxState. graph-desc di-state agent-order) minimax-props)]
    (assoc m :max-val max-val)))

(defn prune-next-states-max
  [{agent-order :agent-order} results {alpha :alpha beta :beta depth :depth}]

  (loop [[res & others] results
         max-val -INF
         cur-alpha alpha]

    (let [v (max max-val (:min-val
                           (assoc-res-with-min-val
                             res agent-order (MiniMaxNodeProps. cur-alpha beta (inc depth)))))]

      (if (or (>= v beta) (nil? others))
        v
        (recur others v (max cur-alpha v))))))

(defn max-value
  [minimax-state minimax-props]

  (let [[agent op-agent] (maximizers-turn minimax-state)]

    (if (term-search? minimax-state minimax-props)
      (let [score (stat-eval minimax-state agent)]
        score)

      (let [ops (next-ops minimax-state op-agent)]
        (let [ops-and-results (assoc-ops-with-res minimax-state op-agent ops)]
          (prune-next-states-max minimax-state ops-and-results minimax-props))))))

(defn prune-next-states-min
  [{agent-order :agent-order} results {alpha :alpha beta :beta depth :depth}]

  (loop [[res & others] results
         min-val INF
         cur-beta beta]

    (let [v (min min-val (:max-val
                           (assoc-res-with-max-val
                             res agent-order (MiniMaxNodeProps. alpha cur-beta (inc depth)))))]

      (if (or (<= v alpha) (nil? others))
        v
        (recur others v (min cur-beta v))))))

(defn min-value
  "The other player plays ->
   will try to choose the min for the first player"
  [minimax-state minimax-props]

  (let [[agent op-agent] (minimizers-turn minimax-state)]

    (if (term-search? minimax-state minimax-props)
      (let [score (stat-eval minimax-state agent)]
        score)

      (let [ops (next-ops minimax-state op-agent)]
        (let [ops-and-results (assoc-ops-with-res minimax-state op-agent ops)]
          (prune-next-states-min minimax-state ops-and-results minimax-props))))))


(defn mini-max [graph-desc di-state agent-order]
  (let [initial-minimax-state (MiniMaxState. graph-desc di-state agent-order)
        initial-minimax-props (MiniMaxNodeProps. -INF INF 0)
        op-agent (first agent-order)]

    (let [ops (next-ops initial-minimax-state op-agent)]
      (let [ops-and-results (assoc-ops-with-res initial-minimax-state op-agent ops)]
        (let [maps-with-min-vals (into [] (map #(assoc-res-with-min-val % agent-order initial-minimax-props) ops-and-results))]

          (doseq [m maps-with-min-vals]
            (log/debug "&>>" (:min-val m) (into {} (:op m))))

          (:op (apply max-key :min-val maps-with-min-vals)))))))