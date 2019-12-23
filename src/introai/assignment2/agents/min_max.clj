(ns introai.assignment2.agents.min-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment2.game-state :as gs]
    [introai.assignment2.agents.utils :refer
     [tail-summary-str calc-agent-score both-agents-terminated progress-tick progress-tick-state]]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
    [introai.assignment2.graph-description :as gd]))


(declare max-value)
(declare min-value)
(declare assoc-res-with-max-val)
(declare assoc-res-with-min-val)
(declare assoc-op-with-res)

(def MAX-DEPTH 20)
(def ALPHA_BETA_PRUNE true)

(defn player-max-sort-key
  "sort key - that will maximize the score of the first agent - second agent"
  [m-ogdmt]
  (-> m-ogdmt :evl :scores ((fn [[p1 p2]] (- p1 p2)))))

(defn player-min-sort-key
  "sort key - that will minimize the score of the first agent - second agent"
  [m-ogdmt]
  (- (player-max-sort-key m-ogdmt)))

(defn own-heuristic
  [{graph-desc :graph-desc di-state :di-state :as minmax-state} agent]
  (agent-heuristic graph-desc di-state agent))

(defrecord MinMaxState [graph-desc di-state heuristic agent-order alpha beta]
  Object
  (toString [x] (str (select-keys x [:agent-order]))))
(defmethod print-method MinMaxState [x ^java.io.Writer w] (.write w (str "MMS:" x)))

(defn make-init-state [graph-desc di-state agent-order]
  (MinMaxState. graph-desc di-state own-heuristic agent-order -INF INF))

(defn inc-depth [{agent-order :agent-order graph-desc :graph-desc di-state :di-state} depth]
  (let [op-agent (first agent-order)
        mid-edge (gutils/node-mid-edge? graph-desc di-state op-agent)
        terminated (agent-term? di-state op-agent)]
    (+ depth (if (or mid-edge terminated) 0 1))))

(defn update-graph-state [minmax-state graph-desc di-state]
  (assoc minmax-state :graph-desc graph-desc :di-state di-state))

(defrecord MinMaxNodeProps [depth time-progressor agent other-agent]
  Object
  (toString [x] (str (select-keys x [:depth :agent]))))
(defmethod print-method MinMaxNodeProps [x ^java.io.Writer w] (.write w (str "Props:" x)))

(defrecord Evaluation [scores h-vals g-vals cutoff depth agent other-agent agent-state id]
  Object
  (toString [x]
    (str (select-keys x [:scores :agent-state]))
    ;(str (into {} x))
    ))
(defmethod print-method Evaluation [x ^java.io.Writer w] (.write w (str "Eval" x)))

(defn make-eval [score-vec h-vec g-vec cutoff minmax-props agent other-agent agent-state]
  (Evaluation. score-vec h-vec g-vec cutoff (:depth minmax-props) (str agent) (str other-agent) agent-state (nano-id 5)))

(defn cutoff-maxmax [minmax-props]
  (log/debug "Depth cutoff!")
  (< MAX-DEPTH (:depth minmax-props)))

(defn term-search? [{di-state :di-state agent-order :agent-order} minmax-props]
  (or
    (both-agents-terminated di-state agent-order)
    (cutoff-maxmax minmax-props)))

(defrecord M-O [op]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-O [x ^java.io.Writer w] (.write w (str "M-O:" x)))

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  "[M-O1 M-O2 ...]"
  (let [n-ops (into [] (map #(M-O. %)
                            (gen-next-ops graph-desc di-state op-agent)))]
    n-ops))

(defrecord M-OGD [op graph-desc di-state]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-OGD [x ^java.io.Writer w] (.write w (str "M-OGD:" x)))
(defn make-m-ogd [m-o graph-desc di-state] (M-OGD. (:op m-o) graph-desc di-state))

(defrecord M-OGDMT [op graph-desc di-state evl tail]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-OGDMT [x ^java.io.Writer w] (.write w (str "M-OGDMT:" x)))
(defn make-m-ogdmt [m-ogd evl tail] (map->M-OGDMT (assoc m-ogd :evl evl :tail tail)))

(defn stat-eval
  "return Evaluation of current state according to agent"
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
        f-score-self (+ (* 1 g-score-self) (:val h-score-self))
        f-score-other (+ (* 1 g-score-other) (:val h-score-other))
        cutoff (not (both-agents-terminated di-state agent-order))]

    (make-eval [f-score-self f-score-other] [h-score-self h-score-other] [g-score-self g-score-other]
               cutoff, minmax-props, agent other-agent agent-state)))

(defn assoc-op-with-res
  "Execute op on current graph and state
  returns:  M-OGD {:op Edge, :graph-dec GraphDescription :di-state TwoAgentState}"
  [graph-desc di-state agent m-o]

  (let [[new-graph-desc new-di-state] ((:op m-o) graph-desc di-state agent)]
    (make-m-ogd m-o new-graph-desc new-di-state)))

(defn compute-ops
  "Execute ops on current graph and state, returns a vector of results
  [M-OGD1 M-OGD2 ...]"
  [{graph-desc :graph-desc di-state :di-state} op-agent m-os]
  (let [m-ogds
        (into []
              (map #(assoc-op-with-res graph-desc di-state op-agent %) m-os))]
    m-ogds))

(defn assoc-res-with-max-val
  "input: op and its result - state & graph desc
  Recursively call max-value and let the other player play
  return: M-OGDMT"
  [{graph-desc :graph-desc di-state :di-state :as m-ogd} minmax-state minmax-props]

  (let [new-minmax-state (update-graph-state minmax-state graph-desc di-state)
        res-and-max-val (max-value new-minmax-state minmax-props)
        m-ogdmt (make-m-ogdmt m-ogd (:evl res-and-max-val) res-and-max-val)]

    m-ogdmt))

(defn assoc-res-with-min-val
  "input: op and its result - state & graph desc
  Recursively call min-value and let the other player play
  return: M-OGDMT"
  [{graph-desc :graph-desc di-state :di-state :as m-ogd} minmax-state minmax-props]

  (let [new-minmax-state (update-graph-state minmax-state graph-desc di-state)
        res-and-min-val (min-value new-minmax-state minmax-props)
        m-ogdmt (make-m-ogdmt m-ogd (:evl res-and-min-val) res-and-min-val)]

    m-ogdmt))

(defn inc-depth-calc-val
  "Update depth and associate with min/max-val recursively"
  [minmax-state minmax-props score-calc m-ogd]

  (let [incr-depth (inc-depth minmax-state (:depth minmax-props))
        m-ogdmt (score-calc
                  m-ogd minmax-state
                  (assoc minmax-props :depth incr-depth))]
    m-ogdmt))

(defn calc-and-prune-larger-beta
  "calc max val for each OGD, until bigger than beta and
  then its useless to cont. because beta will be chosen by the minimizer
  return a vec of M-OGDMT"
  [calc-val minmax-state m-ogd-vec]

  (loop [[m-ogd & others] m-ogd-vec
         m-ogdmt-vec []
         {alpha :alpha beta :beta :as cur-state} minmax-state]

        (if (nil? m-ogd) m-ogdmt-vec

          (let [m-ogdmt (calc-val cur-state m-ogd)
                cur-score (player-max-sort-key m-ogdmt)
                new-alpha-state (assoc cur-state :alpha (max alpha cur-score))
                results (conj m-ogdmt-vec m-ogdmt)]

            (if (>= cur-score beta)
              results
              (recur others results new-alpha-state))))))

(defn calc-and-prune-smaller-alpha
  "calc min val for each OGD, until lesser than alpha and
  then its useless to cont. because alpha will be chosen by the maximizer
  return a vec of M-OGDMT"
  [calc-val minmax-state m-ogd-vec]

  (loop [[m-ogd & others] m-ogd-vec
         m-ogdmt-vec []
         {alpha :alpha beta :beta :as cur-state} minmax-state]

    (if (nil? m-ogd) m-ogdmt-vec

      (let [m-ogdmt (calc-val cur-state m-ogd)
            cur-score (player-max-sort-key m-ogdmt)
            new-beta-state (assoc cur-state :beta (min beta cur-score))
            results (conj m-ogdmt-vec m-ogdmt)]

        (if (<= cur-score alpha)
          results
          (recur others results new-beta-state))))))

(defn calc-m-ogdmt-vec-max
  "Calculate max scores for op results"
  [minmax-state minmax-props m-ogd-vec]
  (let [calc-val #(inc-depth-calc-val %1 minmax-props assoc-res-with-max-val %2)
        calc-val-const-state (partial calc-val minmax-state)]
    (if ALPHA_BETA_PRUNE
      (calc-and-prune-smaller-alpha calc-val minmax-state m-ogd-vec)
      (into [] (map calc-val-const-state m-ogd-vec)))))

(defn calc-m-ogdmt-vec-min
  "Calculate min scores for op results"
  [minmax-state minmax-props m-ogd-vec]
  (let [calc-val #(inc-depth-calc-val %1 minmax-props assoc-res-with-min-val %2)
        calc-val-const-state (partial calc-val minmax-state)]
    (if ALPHA_BETA_PRUNE
      (calc-and-prune-larger-beta calc-val minmax-state m-ogd-vec)
      (into [] (map calc-val-const-state m-ogd-vec)))))

(defn max-m-ogdmt
  "Calculate max scores for op results and return the maximal"
  [minmax-state minmax-props m-ogd-vec]

  (let [m-ogdmt-vec (calc-m-ogdmt-vec-min minmax-state minmax-props m-ogd-vec)
        max-m-ogdmt (last (sort-by player-max-sort-key m-ogdmt-vec))]
    (log/debug "Out of " (into [] (map :evl m-ogdmt-vec)) "the maximal: " (:evl max-m-ogdmt))
    max-m-ogdmt))

(defn min-m-ogdmt
  "Calculate min scores for op results and return the minimal"
  [minmax-state minmax-props m-ogd-vec]

  (let [m-ogdmt-vec (calc-m-ogdmt-vec-max minmax-state minmax-props m-ogd-vec)
        min-m-ogdmt (last (sort-by player-min-sort-key m-ogdmt-vec))]
    (log/debug "Out of " (into [] (map :evl m-ogdmt-vec)) "the minimal: " (:evl min-m-ogdmt))
    min-m-ogdmt))

(defn max-value
  "From a state - run all possible ops recursively
  and return the one with the maximal score
  {:val Evaluation :tail {<previous op> :op :di-state ... :tail}}]}"
  [minmax-state minmax-props]

  (let [[agent op-agent] [(:agent minmax-props) (:agent minmax-props)]]

    (if (term-search? minmax-state minmax-props)
      (let [evl (stat-eval minmax-state minmax-props agent)]
        {:evl evl :tail nil})

      (let [
            ticked-state (progress-tick-state minmax-state minmax-props op-agent)
            ops (next-ops ticked-state op-agent)
            ;ops (next-ops minmax-state op-agent)

            ops-and-results (compute-ops ticked-state op-agent ops)
            ;ops-and-results (compute-ops minmax-state op-agent ops)
            ;ops-and-res-tick (progress-tick minmax-props op-agent ops-and-results)

            ;maximal-m-ogdmt (max-m-ogdmt minmax-state minmax-props ops-and-res-tick)
            maximal-m-ogdmt (max-m-ogdmt ticked-state minmax-props ops-and-results)

            ]

        maximal-m-ogdmt))))

(defn min-value
  "From a state - run all possible ops recursively
  and return the one with the minimal score
  returns: M-OGDMT"
  [minmax-state minmax-props]
  (let [[agent op-agent] [(:agent minmax-props) (:other-agent minmax-props)]]

    (if (term-search? minmax-state minmax-props)
      (let [evl (stat-eval minmax-state minmax-props agent)]
        {:evl evl :tail nil})

      (let [
            ticked-state (progress-tick-state minmax-state minmax-props op-agent)
            ops (next-ops ticked-state op-agent)
            ;ops (next-ops minmax-state op-agent)

            ops-and-results (compute-ops ticked-state op-agent ops)
            ;ops-and-results (compute-ops minmax-state op-agent ops)
            ;ops-and-res-tick (progress-tick minmax-props op-agent ops-and-results)

            ;minimal-m-ogdmt (min-m-ogdmt minmax-state minmax-props ops-and-res-tick)]
            minimal-m-ogdmt (min-m-ogdmt ticked-state minmax-props ops-and-results)
            ]

        minimal-m-ogdmt))))

(defn min-max [graph-desc di-state agent-order]

  (log/info (str (first agent-order)) ">>> " (:remaining-people graph-desc))

  (let [time-progressor (first (filter #(= (:name %) "Bob") agent-order))
        initial-minmax-state (make-init-state graph-desc di-state agent-order)
        initial-minmax-props (MinMaxNodeProps. 0 time-progressor (first agent-order) (second agent-order))
        op-agent (first agent-order)]

    (let [m-o-vec (next-ops initial-minmax-state op-agent)
          m-ogd-vec (compute-ops initial-minmax-state op-agent m-o-vec)
          m-ogdmt-vec (into [] (map #(assoc-res-with-min-val % initial-minmax-state initial-minmax-props) m-ogd-vec))]

      (doseq [m m-ogdmt-vec]
        (log/info (str (first agent-order)) " &>>" (:evl m) (into {} (:op m))
          ;(tail-summary-str m)
          ))

      (:op
        (last (sort-by player-max-sort-key m-ogdmt-vec))))))
