(ns introai.assignment3.agents.mini-max
  (:gen-class)
  (:require
    [introai.assignment3.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment3.agents.utils :refer [tail-summary-str calc-agent-score]]
    [introai.assignment3.game-state :as gs]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
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

(defn inc-depth [{agent-order :agent-order graph-desc :graph-desc di-state :di-state} depth]
  (let [op-agent (first agent-order)
        mid-edge (gutils/node-mid-edge? graph-desc di-state op-agent)
        terminated (agent-term? di-state op-agent)]
    ;(log/info (:name op-agent))
    (+ depth (if (or mid-edge terminated) 0 1))))

(defn progress-tick [minimax-props op-agent ops-and-results]
  (if (not= op-agent (:time-progressor minimax-props))
    ops-and-results
    (map #(update-in % [:di-state :time] inc) ops-and-results)))

(defn update-graph-state [minimax-state graph-desc di-state]
  (assoc minimax-state :graph-desc graph-desc :di-state di-state))

(defrecord MiniMaxNodeProps [alpha beta depth time-progressor]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MiniMaxNodeProps [x ^java.io.Writer w] (.write w (str x)))

(defn both-agents-terminated [di-state agent-order]
  (log/debug "Both agents terminated!")
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

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  (into []
        (map #(hash-map :op %)
             (gen-next-ops graph-desc di-state op-agent))
        )
  )

(defn adversarial-heuristic
  [{graph-desc :graph-desc di-state :di-state :as minimax-state} agent]
  (let [other (gs/other-agent minimax-state agent)
        own-h (agent-heuristic graph-desc di-state agent)
        other-h (agent-heuristic graph-desc di-state other)]
    {:val (- (:val own-h) (:val other-h)) :own own-h :other other-h}
    ))

(defn stat-eval
  "todo: could heuristic be better than actual score?"
  [{di-state :di-state agent-order :agent-order heuristic :heuristic :as minimax-state}
   minimax-props
   agent]

  (let [agent-state (gs/state-of di-state agent)
        g-score (calc-agent-score agent-state)
        h-score (heuristic minimax-state agent)
        cutoff (not (both-agents-terminated di-state agent-order))]

    {:score (+ g-score (:val h-score)) :g g-score :h h-score :cutoff cutoff :depth (:depth minimax-props) :id (nano-id 5)}

    ))

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
        res-and-min-val (min-value new-minimax-state minimax-props)]
    (-> m
        (assoc :min-val (:min-val res-and-min-val))
        (assoc :tail res-and-min-val))
    ;res-and-min-val
    ))

(defn assoc-res-with-max-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} minimax-state minimax-props]

  (let [new-minimax-state (update-graph-state minimax-state graph-desc di-state)
        res-and-max-val (max-value new-minimax-state minimax-props)]
    (-> m
        (assoc :max-val (:max-val res-and-max-val))
        (assoc :tail res-and-max-val))
    ;res-and-max-val
    ))

(defn prune-next-states-max
  [minimax-state ops-and-results {alpha :alpha beta :beta depth :depth time-progressor :time-progressor}]

  (loop [[res & others] ops-and-results
         max-val -INF
         cur-alpha alpha]

    (let [res-and-min-val (assoc-res-with-min-val
                            res minimax-state (MiniMaxNodeProps. cur-alpha beta (inc-depth minimax-state depth) time-progressor))
          v (max max-val (-> res-and-min-val :min-val :score))]

      (if (or (>= v beta) (nil? others))
        ;v
        res-and-min-val
        (recur others v (max cur-alpha v))))))

(defn max-value
  [minimax-state minimax-props]

  (let [[agent op-agent] (maximizers-turn minimax-state)]
    ;(log/info "MAX: " (gs/state-of (:di-state minimax-state) op-agent))

    (if (term-search? minimax-state minimax-props)
      (let [score (stat-eval minimax-state minimax-props agent)]
        {:max-val score :tail nil})

      (let [ops (next-ops minimax-state op-agent)
            ops-and-results (assoc-ops-with-res minimax-state op-agent ops)
            ops-and-res-tick (progress-tick minimax-props op-agent ops-and-results)
            next-op-and-score (prune-next-states-max minimax-state ops-and-res-tick minimax-props)]
        {:max-val (:min-val next-op-and-score) :tail next-op-and-score}
        ))))

(defn prune-next-states-min
  [minimax-state ops-and-results {alpha :alpha beta :beta depth :depth time-progressor :time-progressor}]

  (loop [[res & others] ops-and-results
         min-val INF
         cur-beta beta]

    (let [res-and-max-val (assoc-res-with-max-val
                            res minimax-state (MiniMaxNodeProps. alpha cur-beta (inc-depth minimax-state depth) time-progressor))
          v (min min-val (-> res-and-max-val :max-val :score))]

      (if (or (<= v alpha) (nil? others))
        res-and-max-val
        (recur others v (min cur-beta v))))))

(defn min-value
  "The other player plays ->
   will try to choose the min for the first player"
  [minimax-state minimax-props]

  (let [[agent op-agent] (minimizers-turn minimax-state)]

    (if (term-search? minimax-state minimax-props)
      (let [score (stat-eval minimax-state minimax-props agent)]
        {:min-val score :tail nil})

      (let [ops (next-ops minimax-state op-agent)
            ops-and-results (assoc-ops-with-res minimax-state op-agent ops)
            ops-and-res-tick (progress-tick minimax-props op-agent ops-and-results)
            next-op-and-score (prune-next-states-min minimax-state ops-and-res-tick minimax-props)]
        {:min-val (:max-val next-op-and-score) :tail next-op-and-score}
        ))))

(defn mini-max [graph-desc di-state agent-order]
  (log/info (str (first agent-order)) ">>> " (:remaining-people graph-desc))


  (let [time-progressor (first (filter #(= (:name %) "Bob") agent-order))
        initial-minimax-state (MiniMaxState. graph-desc di-state adversarial-heuristic agent-order)
        initial-minimax-props (MiniMaxNodeProps. -INF INF 0 time-progressor)
        op-agent (first agent-order)]

    (let [ops (next-ops initial-minimax-state op-agent)
          ops-and-results (assoc-ops-with-res initial-minimax-state op-agent ops)
          maps-with-min-vals (into [] (map #(assoc-res-with-min-val % initial-minimax-state initial-minimax-props) ops-and-results))]

      (doseq [m maps-with-min-vals]
        (log/info (str (first agent-order)) " &>>" (:min-val m) (into {} (:op m))
          (tail-summary-str m)
          ))

      (:op (apply max-key #(-> % :min-val :score) maps-with-min-vals)))))