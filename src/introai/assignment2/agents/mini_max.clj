(ns introai.assignment2.agents.mini-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment2.game-state :as gs]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    ))


(declare min-value)
(declare max-value)
(declare assoc-res-with-min-val)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(defrecord MiniMaxState [graph-desc di-state heuristic agent-order]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MiniMaxState [x ^java.io.Writer w] (.write w (str x)))

(defn minimizers-turn [minimax-state] (:agent-order minimax-state))
(defn maximizers-turn [minimax-state] (repeat 2 (first (:agent-order minimax-state))))

(defn update-graph-state [minimax-state graph-desc di-state]
  (assoc minimax-state :graph-desc graph-desc :di-state di-state))

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

(defn stat-eval
  "todo: could heuristic be better than actual score?"
  [{di-state :di-state agent-order :agent-order heuristic :heuristic graph-desc :graph-desc}
   agent]

  (let [agent-state (gs/state-of di-state agent)]

    (if (both-agents-terminated di-state agent-order)
      (calc-agent-score agent-state)
      (heuristic graph-desc di-state agent))))

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
  [{op :op graph-desc :graph-desc di-state :di-state :as m} minimax-state minimax-props]

  (let [new-minimax-state (update-graph-state minimax-state graph-desc di-state)
        min-val (min-value new-minimax-state minimax-props)]
    (assoc m :min-val min-val)))

(defn assoc-res-with-max-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} minimax-state minimax-props]

  (let [new-minimax-state (update-graph-state minimax-state graph-desc di-state)
        max-val (max-value new-minimax-state minimax-props)]
    (assoc m :max-val max-val)))

(defn prune-next-states-max
  [minimax-state ops-and-results {alpha :alpha beta :beta depth :depth}]

  (loop [[res & others] ops-and-results
         max-val -INF
         cur-alpha alpha]

    (let [v (max max-val (:min-val
                           (assoc-res-with-min-val
                             res minimax-state (MiniMaxNodeProps. cur-alpha beta (inc depth)))))]

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
  [minimax-state ops-and-results {alpha :alpha beta :beta depth :depth}]

  (loop [[res & others] ops-and-results
         min-val INF
         cur-beta beta]

    (let [v (min min-val (:max-val
                           (assoc-res-with-max-val
                             res minimax-state (MiniMaxNodeProps. alpha cur-beta (inc depth)))))]

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
  (let [initial-minimax-state (MiniMaxState. graph-desc di-state agent-heuristic agent-order)
        initial-minimax-props (MiniMaxNodeProps. -INF INF 0)
        op-agent (first agent-order)]

    (let [ops (next-ops initial-minimax-state op-agent)]
      (let [ops-and-results (assoc-ops-with-res initial-minimax-state op-agent ops)]
        (let [maps-with-min-vals (into [] (map #(assoc-res-with-min-val % initial-minimax-state initial-minimax-props) ops-and-results))]

          (doseq [m maps-with-min-vals]
            (log/debug "&>>" (:min-val m) (into {} (:op m))))

          (:op (apply max-key :min-val maps-with-min-vals)))))))