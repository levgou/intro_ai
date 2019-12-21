(ns introai.assignment2.agents.min-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment2.game-state :as gs]
    [introai.assignment2.agents.utils :refer [tail-summary-str calc-agent-score]]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
    ))


(declare max-value)
(declare min-value)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(defrecord MinMaxState [graph-desc di-state heuristic agent-order]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MinMaxState [x ^java.io.Writer w] (.write w (str x)))

(defn rev-order [minmax-state]
  (update minmax-state :agent-order reverse))

(defn inc-depth [{agent-order :agent-order graph-desc :graph-desc di-state :di-state} depth]
  (let [op-agent (first agent-order)
        mid-edge (gutils/node-mid-edge? graph-desc di-state op-agent)
        terminated (agent-term? di-state op-agent)]
    (+ depth (if (or mid-edge terminated) 0 1))))

(defn progress-tick [minmax-props op-agent ops-and-results]
  (if (not= op-agent (:time-progressor minmax-props))
    ops-and-results
    (map #(update-in % [:di-state :time] inc) ops-and-results)))

(defn update-graph-state [minmax-state graph-desc di-state]
  "updates graph state with new graph desc and di-state"
  (assoc minmax-state :graph-desc graph-desc :di-state di-state))

(defrecord MinMaxNodeProps [depth maxifier time-progressor agent other-agent]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MinMaxNodeProps [x ^java.io.Writer w] (.write w (str x)))

(defn both-agents-terminated [di-state agent-order]
  (log/debug "Both agents terminated!")
  (and
    (agent-term? di-state (first agent-order))
    (agent-term? di-state (second agent-order))
    ))

(def MAX-DEPTH 9)
(defn cutoff-minmax [minmax-props]
  (log/debug "Depth cutoff!")
  (< MAX-DEPTH (:depth minmax-props)))

(defn term-search? [{di-state :di-state agent-order :agent-order} minmax-props]
  (or
    (both-agents-terminated di-state agent-order)
    (cutoff-minmax minmax-props)))

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  (into []
        (map #(identity {:op %})
             (gen-next-ops graph-desc di-state op-agent))))

(defn own-heuristic
  [{graph-desc :graph-desc di-state :di-state :as minmax-state} agent]
  (let [other (gs/other-agent minmax-state agent)]

    (agent-heuristic graph-desc di-state agent)

    ))

(defn stat-eval
  "todo: could heuristic be better than actual score?"
  [{di-state :di-state agent-order :agent-order heuristic :heuristic :as minmax-state}
   minmax-props
   agent]


  (let [agent-state (gs/state-of di-state agent)
        other-state (gs/other-agent-state minmax-state agent)
        other-agent (gs/other-agent minmax-state agent)

        g-score-self (calc-agent-score agent-state)
        g-score-other (calc-agent-score other-state)
        h-score-self (heuristic minmax-state agent)
        h-score-other (heuristic minmax-state other-agent)
        f-score-self (+ (* 2 g-score-self) (:val h-score-self))
        f-score-other (+ (* 2 g-score-other) (:val h-score-other))
        cutoff (not (both-agents-terminated di-state agent-order))]

    {:score [f-score-self f-score-other] :h-vals [h-score-self h-score-other] :g [g-score-self g-score-other]
     :cutoff cutoff :depth (:depth minmax-props) :id (nano-id 5) :agent (str agent)}
    ))

(defn assoc-op-with-res [graph-desc di-state agent map-with-op]
  "run op - to generate new graph desc and new di-state"
  (let [[new-graph-desc new-di-state] ((:op map-with-op) graph-desc di-state agent)]
    (assoc map-with-op :graph-desc new-graph-desc :di-state new-di-state)))

(defn assoc-ops-with-res
  "run ops to gen new states"
  [{graph-desc :graph-desc di-state :di-state} op-agent ops]
  (into []
        (map #(assoc-op-with-res graph-desc di-state op-agent %) ops)))

(defn assoc-res-with-max-val
  ""
  [{op :op graph-desc :graph-desc di-state :di-state :as m} minmax-state minmax-props]

  (let [new-minmax-state (update-graph-state minmax-state graph-desc di-state)
        res-and-max-val (max-value new-minmax-state minmax-props)]
    (-> m
        (assoc :max-val (:max-val res-and-max-val))
        (assoc :tail res-and-max-val))
    ))

(defn assoc-res-with-min-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} minmax-state minmax-props]

  (let [new-minmax-state (update-graph-state minmax-state graph-desc di-state)
        res-and-min-val (min-value new-minmax-state minmax-props)]
    (-> m
        (assoc :min-val (:min-val res-and-min-val))
        (assoc :tail res-and-min-val))
    ))

(defn current-maxifier [minmax-state minmax-props]
  (if (-> minmax-state :agent-order first :name (= (-> minmax-props :agent :name)))
    (:maxifier1 minmax-props)
    (:maxifier2 minmax-props)))

(defn res-and-max-val [minmax-state minmax-props res]
  (assoc-res-with-max-val
    res minmax-state
    (assoc minmax-props :depth (inc-depth minmax-state (:depth minmax-props)))))

(defn results-and-max-values [minmax-state minmax-props ops-and-results]
  (into [] (map #(res-and-max-val minmax-state minmax-props %) ops-and-results)))

(defn next-state-max
  [minmax-state ops-and-results minmax-props]

  (let [maxifier (:maxifier minmax-props)
        results-and-max-vals (results-and-max-values minmax-state minmax-props ops-and-results)]
    (last (sort-by maxifier results-and-max-vals))))

(defn next-state-min
  [minmax-state ops-and-results minmax-props]

  (let [maxifier (:maxifier minmax-props)
        results-and-max-vals (results-and-max-values minmax-state minmax-props ops-and-results)]
    (last (sort-by maxifier results-and-max-vals))))

(defn max-value
  [minmax-state minmax-props]

  (let [[agent op-agent] [(:agent minmax-props) (:agent minmax-props)]]

    (if (term-search? minmax-state minmax-props)
      (let [di-score (stat-eval minmax-state minmax-props agent)]
        {:max-val di-score :tail nil})

      (let [ops (next-ops minmax-state op-agent)
            ops-and-results (assoc-ops-with-res minmax-state op-agent ops)
            ops-and-res-tick (progress-tick minmax-props op-agent ops-and-results)
            next-op-and-score (next-state-max minmax-state ops-and-res-tick minmax-props)]

        {:max-val (:max-val next-op-and-score) :tail next-op-and-score}
        ))))

(defn min-value
  "The other player plays ->
   will try to choose the min for the first player"
  [minmax-state minmax-props]

  (let [[agent op-agent] [(:agent minmax-props) (:other-agent minmax-props)]]

    (if (term-search? minmax-state minmax-props)
      (let [di-score (stat-eval minmax-state minmax-props agent)]
        {:min-val di-score :tail nil})

      (let [ops (next-ops minmax-state op-agent)
            ops-and-results (assoc-ops-with-res minmax-state op-agent ops)
            ops-and-res-tick (progress-tick minmax-props op-agent ops-and-results)
            next-op-and-score (next-state-min minmax-state ops-and-res-tick minmax-props)]
        {:min-val (:min-val next-op-and-score) :tail next-op-and-score}
        ))))



(defn adversarial-maxifier [res-and-val]
  (-> res-and-val :max-val :score ((juxt first #(- (second %))))))

(defn rev-identity-maxifier [res-and-val]
  (into [] (reverse (adversarial-maxifier res-and-val))))

(defn min-max [graph-desc di-state agent-order]
  (log/debug (str (first agent-order)) ">>> " (:remaining-people graph-desc))

  (let [time-progressor (first (filter #(= (:name %) "Bob") agent-order))
        initial-minmax-state (MinMaxState. graph-desc di-state own-heuristic agent-order)
        initial-minmax-props (MinMaxNodeProps. 0 adversarial-maxifier time-progressor (first agent-order) (second agent-order))
        op-agent (first agent-order)]

    (let [ops (next-ops initial-minmax-state op-agent)
          ops-and-results (assoc-ops-with-res initial-minmax-state op-agent ops)
          maps-with-max-vals (into [] (map #(assoc-res-with-min-val % initial-minmax-state initial-minmax-props) ops-and-results))]

      (doseq [m maps-with-max-vals]
        (log/debug  (str (first agent-order)) " &>>" (:max-val m) (into {} (:op m)) (tail-summary-str m)))

      (:op

        (last (sort-by adversarial-maxifier maps-with-max-vals))

        ))))